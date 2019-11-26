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
	( RawElement(..), RawTexPara(..), RawFootnote(..), RawParagraph(..), LinearSection(..), RawItem(..)
	, loadMacros, parseFile, loadXrefDelta) where

import qualified LaTeXParser as Parser
import qualified Data.Text as Text
import Data.Text (Text, replace)
import Data.Monoid ((<>))
import Document (Row(..), SourceLocation(..), RowSepKind(..), SectionKind(..), Cell(..), CellSpan(..), XrefDelta, Abbreviation)
import Data.Maybe (isJust, fromJust)
import LaTeXParser (Macros(..), Signature(..))
import Data.Text.IO (readFile)
import Text.Regex (mkRegex)
import Data.List (transpose, take)
import Util ((.), (++), mapHead, dropTrailingWs, textStripInfix, textSubRegex, splitOn)
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Control.Arrow (first)
import Data.Char (isSpace)
import LaTeXBase

data RawItem = RawItem
	{ rawItemLabel :: LaTeX
	, rawItemContent :: [RawTexPara] }
	deriving Show

data RawElement
	= RawLatexElement LaTeXUnit
	| RawEnumerated String [RawItem]
	| RawCodeblock LaTeXUnit
	| RawExample [RawTexPara]
	| RawNote Text [RawTexPara]
	| RawBnf String LaTeX
	| RawTable
		{ rawTableCaption :: LaTeX
		, rawColumnSpec :: LaTeX
		, rawTableAbbrs :: [Abbreviation]
		, rawTableBody :: [Row [RawTexPara]] }
	| RawTabbing LaTeX
	| RawFigure { rawFigureName :: LaTeX, rawFigureAbbr :: Abbreviation, rawFigureSvg :: Text }
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
	{ lsectionAbbreviation :: Abbreviation
	, lsectionKind :: SectionKind
	, lsectionName :: LaTeX
	, lsectionParagraphs :: [RawParagraph]
	, lsectionFootnotes :: [RawFootnote] }
	deriving Show

bnfEnvs :: [String]
bnfEnvs = ["bnf", "ncbnf", "bnfkeywordtab", "simplebnf", "ncsimplebnf", "ncrebnf"]

isBnf :: LaTeXUnit -> Bool
isBnf (TeXEnv s _ _)
	| s `elem` bnfEnvs = True
isBnf _ = False

isTable, isTabbing, isFigure :: LaTeXUnit -> Bool
isTable x = isTeXEnv "TableBase" x || isTeXEnv "lib2dtab2" x
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
isJunk (TeXComm "setlength" _) = True
isJunk _ = False

isItem :: LaTeXUnit -> Maybe LaTeX
isItem (TeXComm (dropTrailingWs -> "item") []) = Just []
isItem (TeXComm (dropTrailingWs -> "item") [(_, label)]) = Just label
isItem _ = Nothing

parseItems :: LaTeX -> [RawItem]
parseItems [] = []
parseItems (x : rest)
	| isJunk x = mapHead (mapItemContent (mapHead addJunk)) (parseItems rest)
	| Just label <- isItem x, (item, rest') <- break (isJust . isItem) rest =
		RawItem label (parsePara item) : parseItems rest'
	where
		mapItemContent f (RawItem l c) = RawItem l (f c)
		addJunk :: RawTexPara -> RawTexPara
		addJunk (RawTexPara z) = RawTexPara (dropWhile isOnlySpace $ RawLatexElement x : z)
parseItems _ = error "need items or nothing"

doParse :: Macros -> Text -> (LaTeX, Macros)
doParse m t = (x, y)
	where
		(x, y, []) = Parser.parseString ctx (Text.unpack t)
		ctx = initialContext{Parser.macros=m}

initialContext :: Parser.Context
initialContext = Parser.defaultContext
	{ Parser.dontEval = (bnfEnvs ++) $ words $
			"drawing definition importgraphic itemdescr nontermdef renontermdef defnx outputblock " ++
			"indented note defnote example tabular longtable enumeratea commentellip fref"
	, Parser.kill = ["clearpage", "enlargethispage", "noindent",
			"indent", "vfill", "pagebreak", "!", "-", "glossary",
			"itcorr", "hfill", "nocorr", "small", "kill", "lstset",
			"footnotesize", "rmfamily", "microtypesetup", "@", "ungap", "gramSec", "newcolumntype"]
	, Parser.signatures = signatures }

signatures :: [(String, Signature)]
signatures =
		[(c, Signature i Nothing) | i <- [0..4], c <- words (a i)] ++
		[ ("\n", Signature 0 Nothing)
		, ("item", Signature 0 (Just []))
		, ("nolinebreak", Signature 0 (Just []))
		, ("index", Signature 2 (Just []))
		, ("caption", Signature 2 (Just []))
		, ("gramSec", Signature 2 (Just []))
		]
	where
		a 0 = "today def makeatletter bottomline makeatother Sec left right bmod long prime " ++
			"chapter section paragraph subparagraph fi otextup linebreak newpage log kill " ++
			"textup edef x itcorrwidth itletterwidth small BnfIndent par leq " ++
			"leftmargini BnfInc BnfRest kern protect textsmaller caret sum clearpage " ++
			"xspace onelineskip textlangle textrangle textunderscore tilde raggedright = " ++
			"space copyright textregistered textbackslash hsize makebox nocorr br Gamma " ++
			"frenchspacing list leftmargin listparindent itemindent rmfamily itshape relax " ++
			"nonfrenchspacing endlist upshape ttfamily baselineskip nobreak noindent " ++
			"endfirsthead quad cdot cdots dotsc bnfindentinc footnotemark ldots capsep max min " ++
			"continuedcaption hline endhead footnotesize le times dotsb rightarrow to equiv " ++
			"lfloor rfloor pi geq neq ge lceil rceil ell alpha bigl bigr mu lambda beta " ++
			"tabularnewline exp sigma big delta rho Pi nu infty displaystyle lim sin cos " ++
			"phi int theta zeta FlushAndPrintGrammar hfill break backslash centering " ++
			"normalbaselineskip land lor mapsto normalfont textmu tablerefname figurerefname newline " ++
			"obeyspaces bnfindent vdots tabcolsep columnbreak emergencystretch commentellip " ++
			"gamma widowpenalties sffamily"
		a 1 = "hspace footnote textit textrm textnormal texttt textbf ensuremath ref mbox " ++
			"terminal enlargethispage nontermdef renontermdef textsl textsc textsf text grammarterm term " ++
			"tcode noncxxtcode descr footnotetext microtypesetup cline mathtt mathit mathrm mathsf " ++
			"newcolumntype label newlength uline vspace value newcounter mathscr hyperref " ++
			"phantom sqrt ln emph lstset minipage url indexescape changeglossnumformat " ++
			"removedxref deprxref textsuperscript rlap mathrel mathbin nopnumdiffref fref color"
		a 2 = "pnum addtolength definition defnx addtocounter setcounter frac glossary " ++
			"binom infannex normannex parbox link weblink indexedspan movedxref movedxrefs " ++
			"equal setlength textcolor"
		a 3 = "multicolumn discretionary definecolor deflinkx linkx liblinkx movedxrefii " ++
			"ifthenelse PackageError"
		a 4 = "movedxrefiii"
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
loadFigure f = unsafePerformIO $ do
		dot <- readFile p
		svg <- readProcess "dot" ["-Tsvg", "-Gbgcolor=transparent", "-Nfontsize=13", "-Gfontsize=13"] (Text.unpack $ Text.replace "fontsize=24" "fontsize=13" dot)
		return $ rmIds $ snd $ Text.breakOn "<svg" $ Text.pack svg
	where
		p = Text.unpack $ Text.replace ".pdf" ".dot" f
		r = mkRegex "<g id=\"[^\"]*\"" 
		rmIds = textSubRegex r "<g"
			-- Without rmIds, if a page has more than one figure, it will
			-- have duplicate 'graph1', 'node1', 'edge1' etc ids.

isOnlySpace :: RawElement -> Bool
isOnlySpace (RawLatexElement x) = triml [x] == []
isOnlySpace _ = False

parsePara :: LaTeX -> [RawTexPara]
parsePara u = RawTexPara . dropWhile isOnlySpace . fmap f . splitElems (trim (filter (not . kill) u))
	where
		kill (TeXComm "hline" []) = True
		kill (TeXComm "capsep" []) = True
		kill _ = False
		f :: LaTeXUnit -> RawElement
		f e@(TeXEnv k a stuff)
			| isFigure e
			, [(FixArg, rawFigureName), (FixArg, [TeXRaw rawFigureAbbr]), (FixArg, [TeXRaw figureFile])] <- a
				= RawFigure{rawFigureSvg=loadFigure figureFile, ..}
			| isTable e
			, ((x : _todo) : _) <- lookForCommand "caption" stuff
			, (_, ((FixArg, y) : _), content) : _todo <- matchEnv (`elem` ["tabular", "longtable"]) stuff
				= RawTable
				{ rawTableCaption = snd x
				, rawColumnSpec = y
				, rawTableAbbrs = map (fromJust . isJustRaw . snd . head) (lookForCommand "label" stuff)
				, rawTableBody = breakMultiCols $ parseTable content }
			| isTable e = error $ "other table: " ++ show e
			| isTabbing e = RawTabbing stuff
			| isBnf e = RawBnf k stuff
			| Just ek <- isEnumerate e = RawEnumerated ek (parseItems stuff)
			| isCodeblock e = RawCodeblock e
			| k == "note" || k == "defnote" =
			    let label = case a of [(FixArg, [TeXRaw x])] -> x; _ -> "Note"
			    in RawNote label $ parsePara stuff
			| k == "example" = RawExample $ parsePara stuff
			| k == "itemdecl" || k == "minipage" = RawLatexElement e
		f x = RawLatexElement x
		splitElems :: LaTeX -> [LaTeX]
		splitElems [] = []
		splitElems (x:xs)
			| TeXRaw (textStripInfix "\n\n" -> Just (a, (Text.stripStart -> b))) <- x =
				(if a == "" then ([] :) else ([TeXRaw a] :)) $
					splitElems (if b == "" then xs else TeXRaw b : xs)
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
		collectParas (TeXComm "pnum" [] : more) =
				(\(p : x) -> p{paraNumbered=True, rawParaSourceLoc=Nothing} : x)
				(collectParas more)
		collectParas [] = []
		collectParas x = (RawParagraph False False (parsePara p) Nothing : ps)
			where
				ps = collectParas more
				(p, more) = break isParaEnd x

parseSections :: Int -> LaTeX -> [LinearSection]
parseSections level
	(TeXComm c args : (parseParas -> (lsectionParagraphs, lsectionFootnotes, more)))
	| ((FixArg, isJustRaw -> fromJust -> lsectionAbbreviation), (FixArg, lsectionName), lsectionKind, level') <- case (c, args) of
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

columnBreakCell :: Cell [RawTexPara]
columnBreakCell = Cell Normal [RawTexPara [RawLatexElement (TeXComm "columnbreak" [])]]
isColumnBreakCell :: Cell [RawTexPara] -> Bool
isColumnBreakCell (Cell Normal [RawTexPara [RawLatexElement (TeXComm "columnbreak" [])]]) = True
isColumnBreakCell _ = False

makeRectangular :: a -> [[a]] -> [[a]]
makeRectangular filler rows = (take numCols . (++ repeat filler)) . rows
    where numCols = maximum (length . rows)
        -- Todo: Remove this when the bugs in Chrome's collapsed border rendering are fixed.

breakMultiCols :: [Row [RawTexPara]] -> [Row [RawTexPara]]
    -- implements the multicolfloattable environment's \columnbreak, which is left intact by parseTable
breakMultiCols rows
    | all (\Row{..} -> length cells == 1 && rowSep == NoSep) rows =
        Row NoSep . makeRectangular (Cell Normal []) (transpose $ splitOn isColumnBreakCell $ separateColumnBreaks $ (head . cells) . rows)
    | otherwise = rows
    where
        separateColumnBreaks :: [Cell [RawTexPara]] -> [Cell [RawTexPara]]
        separateColumnBreaks = concatMap f
            where
                f :: Cell [RawTexPara] -> [Cell [RawTexPara]]
                f c@Cell{..} | [RawTexPara (RawLatexElement (TeXComm "columnbreak" []) : rest)] <- content =
                        [columnBreakCell, c{content = [RawTexPara rest]}]
                    | otherwise = [c]

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

rmExplSyntax :: Text -> Text
rmExplSyntax = Text.unlines . f . Text.lines
    where
        f [] = []
        f ("\\ExplSyntaxOn" : (dropWhile (/= "\\ExplSyntaxOff") -> (_ : x))) = f x
        f (h : t) = h : f t

loadMacros :: IO Macros
loadMacros =
	snd
	. doParse mempty
	. replace "\\newcommand{\\cv}{\\ifmmode\\mathit{cv}\\else\\cvqual{cv}\\fi}" "\\newcommand{\\cv}{\\mathit{cv}}"
	. replace
	       "\\renewcommand{\\tref}[1]{\\hyperref[tab:#1]{\\tablerefname \\nolinebreak[3] \\ref*{tab:#1}}}\n"
	       "\\renewcommand{\\tref}[1]{Table \\ref{tab:#1}}\n"
	. replace "\\indeximpldef{" "\\index[impldefindex]{"
	. textSubRegex (mkRegex "\\\\penalty[0-9]+") ""
	. ("\\newcommand{\\texorpdfstring}[2]{#2}\n" ++)
	. rmExplSyntax
	. mconcat
	. mapM readFile
	["config.tex", "macros.tex", "tables.tex"]

loadXrefDelta :: IO XrefDelta
loadXrefDelta = do
	(tex, _, _) <- Parser.parseString initialContext . Text.unpack . replace "\\dcr" "--" . readFile "xrefdelta.tex"
	let lfc c = lookForCommand c tex
	return $
		[ (fromJust $ isJustRaw $ snd from, [snd to])
			| [from, to] <- lfc "movedxrefs" ] ++
		[ (fromJust $ isJustRaw $ snd from, (:[]) . TeXComm "ref" . (:[]) . tos)
			| from : tos <- lfc "movedxref" ++ lfc "movedxrefii" ++ lfc "movedxrefiii" ] ++
		[ (abbr, [])
			| [(_, [TeXRaw abbr])] <- lfc "removedxref" ] ++
		[ (abbr, [[TeXComm "ref" [(FixArg, [TeXRaw ("depr." ++ abbr)])]]])
			| [(_, [TeXRaw abbr])] <- lfc "deprxref" ]
