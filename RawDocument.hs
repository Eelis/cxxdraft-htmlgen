{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	ViewPatterns,
	LambdaCase,
	TupleSections,
	NamedFieldPuns,
	FlexibleInstances,
	FlexibleContexts,
	RankNTypes,
	MultiParamTypeClasses,
	FunctionalDependencies,
	UndecidableInstances,
	RecursiveDo #-}

module RawDocument
	( RawElement(..), RawTexPara(..), RawFootnote(..), RawParagraph(..), LinearSection(..)
	, loadMacros, parseFile) where

import qualified LaTeXParser as Parser
import qualified Data.Text as Text
import Data.Text (Text, replace)
import Data.Monoid ((<>))
import Document (Row(..), SourceLocation(..), RowSepKind(..), SectionKind(..), Cell(..), CellSpan(..))
import Data.Maybe (isJust)
import LaTeXParser (Macros(..), Signature(..))
import Data.Text.IO (readFile)
import Text.Regex (mkRegex)
import Util ((.), (++), mapHead, dropTrailingWs, textStripInfix, textSubRegex)
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Control.Arrow (first)
import Data.Char (isSpace)
import LaTeXBase

data RawElement
	= RawLatexElement LaTeXUnit
	| RawEnumerated String [[RawTexPara]]
	| RawCodeblock LaTeXUnit
	| RawExample [RawTexPara]
	| RawNote [RawTexPara]
	| RawBnf String LaTeX
	| RawTable
		{ rawTableCaption :: LaTeX
		, rawColumnSpec :: LaTeX
		, rawTableAbbrs :: [LaTeX]
		, rawTableBody :: [Row [RawTexPara]] }
	| RawTabbing LaTeX
	| RawFigure { rawFigureName :: LaTeX, rawFigureAbbr :: LaTeX, rawFigureSvg :: Text }
	deriving Show

newtype RawTexPara = RawTexPara { rawTexParaElems :: [RawElement] }
	deriving Show

newtype RawFootnote = RawFootnote [RawTexPara]
	deriving Show

data RawParagraph = RawParagraph
	{ paraNumbered :: Bool
	, rawParaInItemdescr :: Bool
	, rawParaElems :: [RawTexPara]
	, rawParaSourceLoc :: Maybe SourceLocation }
	deriving Show

data LinearSection = LinearSection
	{ lsectionAbbreviation :: LaTeX
	, lsectionKind :: SectionKind
	, lsectionName :: LaTeX
	, lsectionParagraphs :: [RawParagraph]
	, lsectionFootnotes :: [RawFootnote] }
	deriving Show

bnfEnvs :: [String]
bnfEnvs = ["bnf", "ncbnf", "bnfkeywordtab", "bnftab", "ncbnftab", "simplebnf", "ncsimplebnf"]

isBnf :: LaTeXUnit -> Bool
isBnf (TeXEnv s _ _)
	| s `elem` bnfEnvs = True
isBnf _ = False

isTable, isTabbing, isFigure :: LaTeXUnit -> Bool
isTable = isTeXEnv "TableBase"
isTabbing = isTeXEnv "tabbing"
isFigure = isTeXEnv "importgraphic"

isEnumerate :: LaTeXUnit -> Maybe String
isEnumerate (TeXEnv s _ _)
	| s `elem` ["enumeratea", "enumerate", "itemize", "description"] = Just s
isEnumerate _ = Nothing

isParaEnd :: LaTeXUnit -> Bool
isParaEnd (TeXEnv "indexed" _ (x:_)) = isParaEnd x
isParaEnd (TeXEnv "itemdecl" _ _) = True
isParaEnd (TeXEnv "itemdescr" _ _) = True
isParaEnd (TeXComm "pnum" _) = True
isParaEnd x = isParasEnd x

isParasEnd :: LaTeXUnit -> Bool
isParasEnd (TeXComm "definition" _) = True
isParasEnd (TeXComm "rSec" _) = True
isParasEnd (TeXComm "infannex" _) = True
isParasEnd (TeXComm "normannex" _) = True
isParasEnd _ = False

isJunk :: LaTeXUnit -> Bool
isJunk (TeXRaw x) = all isSpace (Text.unpack x)
isJunk (TeXComm "index" _) = True
isJunk _ = False

isItem :: LaTeXUnit -> Bool
isItem (TeXComm (dropTrailingWs -> "item") _) = True
isItem _ = False

parseItems :: LaTeX -> [[RawTexPara]]
parseItems [] = []
parseItems (x : rest)
	| isJunk x = mapHead (mapHead addJunk) (parseItems rest)
	| isItem x, (item, rest') <- break isItem rest = parsePara item : parseItems rest'
	where
		addJunk :: RawTexPara -> RawTexPara
		addJunk (RawTexPara z) = RawTexPara (RawLatexElement x : z)
parseItems _ = error "need items or nothing"

doParse :: Macros -> Text -> (LaTeX, Macros)
doParse m t = (x, y)
	where
		(x, y, []) = Parser.parseString ctx (Text.unpack t)
		ctx = initialContext{Parser.macros=m}

initialContext :: Parser.Context
initialContext = Parser.defaultContext
	{ Parser.dontEval = (bnfEnvs ++) $ words $
			"drawing definition importgraphic bottomline capsep itemdescr " ++
			"grammarterm nontermdef defnx FlushAndPrintGrammar term caret indented note example " ++
			"tabular longtable enumeratea emph link linkx liblinkx weblink deflinkx indexedspan"
	, Parser.kill = ["clearpage", "enlargethispage", "noindent",
			"indent", "vfill", "pagebreak", "!", "-", "glossary",
			"itcorr", "hfill", "nocorr", "small", "kill", "lstset",
			"footnotesize", "rmfamily", "microtypesetup", "@", "ungap", "gramSec", "newcolumntype"]
	, Parser.signatures = signatures }

signatures :: [(String, Signature)]
signatures =
		[(c, Signature i Nothing) | i <- [0..3], c <- words (a i)] ++
		[ ("\n", Signature 0 Nothing)
		, ("index", Signature 2 (Just []))
		, ("caption", Signature 2 (Just []))
		, ("gramSec", Signature 2 (Just []))
		]
	where
		a 0 = "today item def makeatletter bottomline makeatother Sec left right bmod " ++
			"chapter section paragraph subparagraph fi otextup linebreak newpage log kill " ++
			"textup edef x itcorrwidth itletterwidth small BnfIndent setlength par leq " ++
			"leftmargini BnfInc BnfRest kern protect textsmaller caret sum clearpage " ++
			"xspace onelineskip textlangle textrangle textunderscore tilde raggedright = " ++
			"space copyright textregistered textbackslash hsize makebox nocorr br Gamma " ++
			"frenchspacing list leftmargin listparindent itemindent rmfamily itshape relax " ++
			"color nonfrenchspacing endlist upshape ttfamily baselineskip nobreak noindent " ++
			"endfirsthead quad cdot cdots dotsc bnfindentinc footnotemark ldots capsep max min " ++
			"continuedcaption hline endhead footnotesize le times dotsb rightarrow to equiv " ++
			"lfloor rfloor pi geq neq ge lceil rceil ell alpha bigl bigr mu lambda beta " ++
			"tabularnewline exp sigma big delta rho Pi nu infty displaystyle lim sin cos " ++
			"phi int theta zeta FlushAndPrintGrammar hfill break backslash centering " ++
			"normalbaselineskip"
		a 1 = "hspace footnote textit textrm textnormal texttt textbf ensuremath ref mbox " ++
			"terminal enlargethispage nontermdef textsl textsc text grammarterm term " ++
			"tcode descr footnotetext microtypesetup cline mathtt mathit mathrm mathsf " ++
			"newcolumntype label newlength uline vspace value newcounter mathscr " ++
			"phantom sqrt ln emph lstset minipage url"
		a 2 = "pnum addtolength definition defnx addtocounter setcounter frac glossary " ++
			"binom infannex normannex parbox link weblink indexedspan"
		a 3 = "multicolumn discretionary definecolor deflinkx linkx liblinkx"
		a _ = undefined

parseFile :: Macros -> Text -> [LinearSection]
parseFile macros =
	parseSections 0
	. fst
	. doParse macros
	. replace "$$" "$"
	. replace "\\hspace*" "\\hspace"
	. replace "``" "“"
	. replace "''" "”"

	. textSubRegex (mkRegex "(\\grammarterm\\{[A-Za-z-]*\\})\\{s\\}") "\\1\\textit{s}"
		-- Mixing italic and upright looks okay in the PDF, but looks bad in browsers,
		-- and our linkification makes clear enough that the plural 's' is not part
		-- of the grammarterm.

loadFigure :: Text -> Text
loadFigure f =
		rmIds $ snd $ Text.breakOn "<svg" $ Text.pack
			$ unsafePerformIO (readProcess "dot" ["-Tsvg", "-Gbgcolor=transparent", p] "")
	where
		p = Text.unpack $ Text.replace ".pdf" ".dot" f
		r = mkRegex "<g id=\"[^\"]*\"" 
		rmIds = textSubRegex r "<g"
			-- Without rmIds, if a page has more than one figure, it will
			-- have duplicate 'graph1', 'node1', 'edge1' etc ids.

parsePara :: LaTeX -> [RawTexPara]
parsePara u = RawTexPara . fmap f . splitElems (trim u)
	where
		f :: LaTeXUnit -> RawElement
		f e@(TeXEnv k a stuff)
			| isFigure e
			, [(FixArg, rawFigureName), (FixArg, rawFigureAbbr), (FixArg, [TeXRaw figureFile])] <- a
				= RawFigure{rawFigureSvg=loadFigure figureFile, ..}
			| isTable e
			, ((x : _todo) : _) <- lookForCommand "caption" stuff
			, (_, ((FixArg, y) : _), content) : _todo <- matchEnv (`elem` ["tabular", "longtable"]) stuff
				= RawTable
				{ rawTableCaption = snd x
				, rawColumnSpec = y
				, rawTableAbbrs = map (snd . head) (lookForCommand "label" stuff)
				, rawTableBody = parseTable content }
			| isTable e = error $ "other table: " ++ show e
			| isTabbing e = RawTabbing stuff
			| isBnf e = RawBnf k stuff
			| Just ek <- isEnumerate e = RawEnumerated ek (parseItems stuff)
			| isCodeblock e = RawCodeblock e
			| k == "note" = RawNote $ parsePara stuff
			| k == "example" = RawExample $ parsePara stuff
			| k == "itemdecl" || k == "minipage" = RawLatexElement e
		f x = RawLatexElement x
		splitElems :: LaTeX -> [LaTeX]
		splitElems [] = []
		splitElems (x:xs)
			| TeXRaw (textStripInfix "\n\n" -> Just (a, (Text.dropWhile isSpace -> b))) <- x =
				(if a == "" then id else ([TeXRaw a] :)) $ case splitElems xs of
					[] -> if b /= "" then [[TeXRaw b]] else []
					v:w ->  ((if b /= "" then (TeXRaw b :) else id) v) : w
			| otherwise = case splitElems xs of
				[] -> [[x]]
				a:b -> ((x:a):b)

class ExtractFootnotes a where extractFootnotes :: a -> (a, [RawFootnote])

instance ExtractFootnotes a => ExtractFootnotes [a] where
	extractFootnotes l = (map fst x, x >>= snd)
		where x = extractFootnotes . l

instance ExtractFootnotes LaTeXUnit where
	extractFootnotes (TeXComm "footnote" [(_, content)]) =
		(TeXComm "footnoteref" [], [RawFootnote $ parsePara content])
	extractFootnotes (TeXComm "footnotemark" []) =
		(TeXComm "footnoteref" [], [])
	extractFootnotes (TeXComm "footnotetext" [(_, content)]) =
		(TeXRaw "" {- todo.. -}, [RawFootnote $ parsePara content])
	extractFootnotes (TeXComm a [(FixArg, content)]) =
		first (\c -> TeXComm a [(FixArg, c)]) (extractFootnotes content)
	extractFootnotes (TeXEnv env args content) = first (TeXEnv env args) (extractFootnotes content)
	extractFootnotes other = (other, [])

parseParas :: LaTeX -> ([RawParagraph], [RawFootnote], LaTeX {- rest -})
parseParas (break isParasEnd -> (extractFootnotes -> (stuff, fs), rest))
		= (collectParas stuff, fs, rest)
	where
		collectParas :: LaTeX -> [RawParagraph]
		collectParas (t@(TeXEnv "itemdecl" _ _) : more) =
			RawParagraph False False (parsePara [t]) Nothing : collectParas more
		collectParas (TeXEnv "itemdescr" _ desc : more) =
			map (\p -> p{rawParaInItemdescr=True}) (collectParas desc)
			++ collectParas more
		collectParas (TeXComm "pnum"
			[ (FixArg, [TeXRaw (Text.unpack -> file)])
			, (FixArg, [TeXRaw (Text.unpack -> read -> lineNr)])] : more) =
				(\(p : x) -> p{paraNumbered=True, rawParaSourceLoc=Just (SourceLocation file lineNr)} : x)
				(collectParas more)
		collectParas [] = []
		collectParas x = (RawParagraph False False (parsePara p) Nothing : ps)
			where
				ps = collectParas more
				(p, more) = break isParaEnd x

parseSections :: Int -> LaTeX -> [LinearSection]
parseSections level
	(TeXComm c args : (parseParas -> (lsectionParagraphs, lsectionFootnotes, more)))
	| ((FixArg, lsectionAbbreviation), (FixArg, lsectionName), lsectionKind, level') <- case (c, args) of
		("normannex", [abbr, name]) -> (abbr, name, NormativeAnnexSection, level)
		("infannex", [abbr, name]) -> (abbr, name, InformativeAnnexSection, level)
		("definition", [name, abbr]) -> (abbr, name, DefinitionSection (level + 1), level)
		("rSec", [(FixArg, [TeXRaw (Text.unpack -> read -> l)]), abbr, name]) ->
			(abbr, name, NormalSection l, l)
		_ -> error $ "not a section command: " ++ show (c, args)
	= LinearSection{..} : parseSections level' more
parseSections _ [] = []
parseSections l (x:xx)
	| TeXRaw t <- x, all isSpace (Text.unpack t) = parseSections l xx
	| otherwise = error $ "parseSections: " ++ show x

parseTable :: LaTeX -> [Row [RawTexPara]]
parseTable [] = []
parseTable latex
	| row == [] = parseTable $ tail rest
	| hasCommand (== "endfirsthead") row = parseTable $ findEndHead rest
	| hasCommand (`elem` ["caption", "bottomline"]) row = parseTable rest
	| otherwise = makeRow row : parseTable rest
	where
		(row, rest) = break (== TeXLineBreak) latex
		findEndHead l
			| row' == [] = findEndHead $ tail rest'
			| hasCommand (== "endhead") row' = l
			| otherwise = findEndHead rest'
			where
				(row', rest') = break (== TeXLineBreak) l

makeRow :: LaTeX -> Row [RawTexPara]
makeRow l = Row sep $ makeRowCells l
	where
		sep
			| hasCommand (== "hline") l = RowSep
			| hasCommand (== "capsep") l = CapSep
			| hasCommand (== "cline") l = Clines $ clines $ lookForCommand "cline" l
			| otherwise = NoSep

		clines [] = []
		clines (([(FixArg, [TeXRaw c])]) : rest) = (begin, end) : clines rest
			where
				(begin', end') = Text.breakOn "-" c
				begin = read $ Text.unpack begin' :: Int
				end = read $ Text.unpack $ Text.tail end' :: Int
		clines other = error $ "Unexpected \\clines syntax: " ++ show other

makeRowCells :: LaTeX -> [Cell [RawTexPara]]
makeRowCells [] = []
makeRowCells latex =
	case rest of
		[] -> [makeCell cell]
		_ : r ->
			(makeCell $ cell <> [TeXRaw cell']) : makeRowCells (TeXRaw rest'' : r)
	where
		(cell, rest) = break isColEnd latex
		isColEnd (TeXRaw c) = isJust $ Text.find (== '&') c
		isColEnd _ = False

		(cell', rest') = Text.break (== '&') $ getText rest
		rest'' = Text.drop 1 rest'
		getText (TeXRaw s : _) = s
		getText other = error $ "Didn't expect " ++ show other

		makeCell content
			| [[(FixArg, [TeXRaw w]), (FixArg, cs), (FixArg, content')]] <- lookForCommand "multicolumn" content =
				Cell (Multicolumn (read $ Text.unpack w) cs) $ parsePara content'
			| otherwise =
				Cell Normal $ parsePara content

loadMacros :: IO Macros
loadMacros =
	snd
	. doParse mempty
	. replace "\\indeximpldef{" "\\index[impldefindex]{"
	. textSubRegex (mkRegex "\\\\penalty[0-9]+") ""
	. ("\\newcommand{\\texorpdfstring}[2]{#2}\n" ++)
	. mconcat
	. mapM readFile
	["config.tex", "macros.tex", "tables.tex"]
