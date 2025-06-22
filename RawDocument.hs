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
	, loadMacros, parseFile, loadXrefDelta, doParse) where

import qualified LaTeXParser as Parser
import qualified Data.Text as Text
import Data.Text (Text, replace)
import Document (Row(..), SourceLocation(..), RowSepKind(..), SectionKind(..), Cell(..), CellSpan(..), XrefDelta, Abbreviation, ColumnSpec(..), TextAlignment(..))
import Data.Maybe (isJust, fromJust)
import LaTeXParser (Macros(..), Signature(..), nullCmd, storeCmd, storeEnv, Environment(..), Command(..), codeEnv, Token(..), normalCmd, ParseResult(..))
import Data.Text.IO (readFile)
import Text.Regex (mkRegex)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (transpose, take, isPrefixOf)
import Util ((.), (++), mapHead, textStripInfix, textSubRegex, splitOn)
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Control.Arrow (first)
import Data.Char (isSpace, isDigit)
import LaTeXBase

data RawItem = RawItem
	{ rawItemLabel :: LaTeX
	, rawItemContent :: [RawTexPara] }
	deriving (Eq, Show)

data RawElement
	= RawLatexElement LaTeXUnit
	| RawEnumerated String [RawItem]
	| RawCodeblock LaTeXUnit
	| RawExample [RawTexPara]
	| RawNote Text [RawTexPara]
	| RawItemdescr [RawTexPara]
	| RawBnf String LaTeX
	| RawTable
		{ rawTableCaption :: LaTeX
		, rawColumnSpec :: [ColumnSpec]
		, rawTableAbbr :: Abbreviation
		, rawTableBody :: [Row [RawTexPara]] }
	| RawTabbing LaTeX
	| RawFormula { rawFormulaAbbr :: Abbreviation, rawFormulaContent :: LaTeX }
	| RawFigure { rawFigureName :: LaTeX, rawFigureAbbr :: Abbreviation, rawFigureSvg :: Text }
	deriving (Eq, Show)

newtype RawTexPara = RawTexPara { rawTexParaElems :: [RawElement] }
	deriving (Eq, Show)

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

instance AllUnits RawElement where
	allUnits (RawLatexElement x) = allUnits x
	allUnits (RawBnf _ x) = allUnits x
	allUnits (RawTabbing x) = allUnits x
	allUnits (RawNote _ x) = allUnits x
	allUnits (RawExample x) = allUnits x
	allUnits (RawCodeblock x) = allUnits x
	allUnits (RawItemdescr x) = allUnits x
	allUnits (RawEnumerated _ x) = allUnits x
	allUnits (RawFormula _ x) = allUnits x
	allUnits RawFigure{} = []
	allUnits RawTable{..} = allUnits rawTableCaption ++ concatMap (allUnits . concat . map content) (map cells rawTableBody)

instance AllUnits RawTexPara where
	allUnits = allUnits . rawTexParaElems

instance AllUnits RawItem where
	allUnits RawItem{..} = allUnits rawItemLabel ++ allUnits rawItemContent

instance AllUnits LinearSection where
	allUnits LinearSection{..} = allUnits lsectionName ++ allUnits lsectionParagraphs ++ allUnits lsectionFootnotes

instance AllUnits RawParagraph where
	allUnits RawParagraph{..} = allUnits rawParaElems

instance AllUnits RawFootnote where
	allUnits (RawFootnote x) = allUnits x

bnfEnvs :: [String]
bnfEnvs = ["bnf", "ncbnf", "bnfkeywordtab", "simplebnf", "ncsimplebnf", "ncrebnf"]

isBnf :: LaTeXUnit -> Bool
isBnf (TeXEnv s _ _)
	| s `elem` bnfEnvs = True
isBnf _ = False

isTable, isTabbing, isFigure :: LaTeXUnit -> Bool
isTable x = isTeXEnv "floattablebasex" x || isTeXEnv "htmlTable" x
isTabbing = isTeXEnv "tabbing"
isFigure = isTeXEnv "importgraphic"

isEnumerate :: LaTeXUnit -> Maybe String
isEnumerate (TeXEnv s _ _)
	| s `elem` ["enumerate", "itemize", "description", "thebibliography"] = Just s
isEnumerate _ = Nothing

isParaEnd :: LaTeXUnit -> Bool
isParaEnd (TeXEnv "itemdecl" _ _) = True
isParaEnd (TeXEnv "indexeditemdecl" _ _) = True
isParaEnd (TeXEnv "itemdescr" _ _) = True
isParaEnd (TeXComm "pnum" _ _) = True
isParaEnd x = isParasEnd x

isParasEnd :: LaTeXUnit -> Bool
isParasEnd (TeXComm "definition" _ _) = True
isParasEnd (TeXComm "rSec" _ _) = True
isParasEnd (TeXComm "infannex" _ _) = True
isParasEnd (TeXComm "normannex" _ _) = True
isParasEnd _ = False

isJunk :: LaTeXUnit -> Bool
isJunk (TeXRaw x) = all isSpace (Text.unpack x)
isJunk (TeXComm "index" _ _) = True
isJunk (TeXComm "setlength" _ _) = True
isJunk _ = False

isItem :: LaTeXUnit -> Maybe LaTeX
isItem (TeXComm "item" _ []) = Just []
isItem (TeXComm "item" _ [(_, label)]) = Just label
isItem (TeXComm "bibitem" _ [(_, [TeXRaw label])]) = Just [TeXRaw $ "bib:" ++ label]
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

nullCmds :: [(Int, String)]
nullCmds =
	[ (0, "clearpage kill rmfamily hfill vfill nocorr small larger noindent itcorrwidth itletterwidth global bigskip begingroup endgroup")
	, (1, "enlargethispage lstset newsavebox vspace input thispagestyle")
	, (2, "glossary settowidth addtolength copypagestyle")
	, (3, "definecolor makeheadrule")
	, (4, "makeoddhead")
	]

storeCmds :: [(Int, String)]
storeCmds =
	[ (0, "today def makeatletter bottomline makeatother Sec bmod mod long prime " ++
			"chapter section paragraph subparagraph fi otextup linebreak newpage log " ++
			"textup edef x BnfIndent par leq " ++
			"leftmargini BnfInc BnfRest protect caret sum " ++
			"xspace onelineskip textlangle textrangle tilde raggedright = " ++
			"space copyright textregistered textbackslash hsize br Gamma " ++
			"frenchspacing list leftmargin listparindent itemindent itshape relax " ++
			"nonfrenchspacing endlist upshape ttfamily baselineskip nobreak " ++
			"endfirsthead quad qquad cdot cdots dotsc bnfindentinc footnotemark ldots capsep max min " ++
			"continuedcaption hline endhead footnotesize le times dotsb rightarrow to equiv " ++
			"lfloor rfloor pi geq neq ge lceil rceil ell alpha bigl bigr mu lambda beta " ++
			"tabularnewline exp sigma big delta rho Pi nu infty displaystyle lim sin cos " ++
			"phi int theta zeta FlushAndPrintGrammar break backslash centering " ++
			"normalbaselineskip land lor mapsto normalfont textmu tablerefname figurerefname newline " ++
			"obeyspaces bnfindent vdots tabcolsep columnbreak emergencystretch commentellip " ++
			"gamma widowpenalties sffamily parskip left right `")
	, (1, "hspace footnote textit textrm textnormal texttt textbf ensuremath ref ref* mbox bibitem mathop " ++
			"terminal literalterminal noncxxterminal textsl textsc textsf text term overline " ++
			"tcode noncxxtcode literaltcode footnotetext microtypesetup cline mathtt mathit mathrm mathsf " ++
			"label newlength uline value newcounter mathscr c uppercase iref operatorname textlarger " ++
			"phantom hphantom sqrt ln emph minipage url indexescape changeglossnumformat textasciitilde " ++
			"removedxref deprxref textsuperscript rlap mathrel mathbin nopnumdiffref color ucode uname")
	, (2, "pnum definition addtocounter setcounter frac " ++
			"binom infannex normannex parbox link weblink indexedspan movedxref movedxrefs " ++
			"equal setlength textcolor")
	, (3, "multicolumn discretionary movedxrefii ifthenelse PackageError NewEnviron")
	, (4, "movedxrefiii indexlink hiddenindexlink")
	]

initialCmds :: Map Text Command
initialCmds = Map.fromList $
	[ storeCmd "item" (Signature 0 (Just []))
	, storeCmd "caption" (Signature 2 (Just []))
	, storeCmd "index" (Signature 2 (Just []))
	, storeCmd "hyperref" (Signature 2 (Just []))
	, nullCmd "makebox" (Signature 2 (Just []))
	, storeCmd "\n" (Signature 0 Nothing)
	, storeCmd "nolinebreak" (Signature 0 (Just []))
	, storeCmd "textsmaller" (Signature 2 (Just []))
	, nullCmd "gramSec" (Signature 2 (Just []))
	, ("kern", normalCmd $ Command $ \_ctx _ws -> ParseResult [] mempty . snd . parseDimen)
	]
	++ [storeCmd c (Signature a Nothing) | (a, l) <- storeCmds, c <- words l]
	++ [nullCmd (Text.pack c) (Signature a Nothing) | (a, l) <- nullCmds, c <- words l]

parseDimen :: [Token] -> ([Token], [Token])
parseDimen toks
    | t@(Token txt) : more <- toks, txt `elem` [".", "pt", "-", "em"] || all isDigit txt = first (t :) (parseDimen more)
    | otherwise = ([], toks)

initialEnvs :: Map Text Environment
initialEnvs = Map.fromList $
	[ (storeEnv e (Signature 0 Nothing))
	| e <- bnfEnvs ++
	       words "indented description itemize center tabbing defnote enumerate eqnarray* equation* itemdescr footnote matrix"
	] ++
	[ storeEnv "example" (Signature 1 (Just []))
	, storeEnv "tailexample" (Signature 1 (Just []))
	, storeEnv "note" (Signature 0 (Just [Token "Note"]))
	, storeEnv "tailnote" (Signature 0 (Just [Token "Note"]))
	, storeEnv "table" (Signature 1 Nothing)
	, storeEnv "tabular" (Signature 1 Nothing)
	, storeEnv "longtable" (Signature 1 Nothing)
	, storeEnv "importgraphic" (Signature 3 Nothing)
	, storeEnv "formula" (Signature 1 Nothing)
	, storeEnv "minipage" (Signature 1 Nothing)
	, storeEnv "thebibliography" (Signature 1 Nothing)
	, codeEnv "indexeditemdecl" (Signature 1 Nothing)
	, codeEnv "itemdecl" (Signature 0 Nothing)
	, codeEnv "indexedcodeblock" (Signature 1 Nothing)
	, codeEnv "codeblock" (Signature 0 Nothing)
	, codeEnv "codeblockdigitsep" (Signature 0 Nothing)
	, codeEnv "codeblocktu" (Signature 1 Nothing)
	, storeEnv "array" (Signature 1 Nothing)
	, storeEnv "floattablebasex" (Signature 4 Nothing)
	, storeEnv "htmlTable" (Signature 3 Nothing)
	]

initialMacros :: Parser.Macros
initialMacros = Parser.defaultMacros ++ mempty{Parser.commands=initialCmds, Parser.environments=initialEnvs}

initialContext :: Parser.Context
initialContext = Parser.defaultContext{Parser.macros=initialMacros}

parseFile :: Macros -> Text -> ([LinearSection], Macros)
parseFile macros =
	first (parseSections 0)
	. doParse macros
	. replace "$$" "$"
	. replace "\\hspace*" "\\hspace"
	. replace "``" "“"
	. textSubRegex (mkRegex "(\\grammarterm\\{[A-Za-z-]*\\})(\\{s\\}|s)") "\\1\\textit{s}"
		-- Mixing italic and upright looks okay in the PDF, but looks bad in browsers,
		-- and our linkification makes clear enough that the plural 's' is not part
		-- of the grammarterm.

loadFigure :: Text -> Text
loadFigure f = unsafePerformIO $ do
		dot <- readFile $ "assets/" ++ p
		svg <- readProcess "dot" ["-Tsvg",
			"-Gbgcolor=transparent",
			"-Gsize=8",
			"-Nfontsize=10",
			"-Gfontsize=10",
			"-Efontsize=10",
			"-Nfontname=Noto Serif",
			"-Efontname=Noto Serif",
			"-Gfontname=Noto Serif"] (Text.unpack $ Text.replace "Courier New" "Noto Sans Mono" $ Text.replace ", fontsize=24" "" dot)
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
		kill (TeXComm "hline" _ []) = True
		kill (TeXComm "capsep" _ []) = True
		kill (TeXComm "endhead" _ _) = True
		kill _ = False
		f :: LaTeXUnit -> RawElement
		f e@(TeXEnv k a stuff)
			| isFigure e
			, [(FixArg, rawFigureName), (FixArg, [TeXRaw rawFigureAbbr]), (FixArg, [TeXRaw figureFile])] <- a
				= RawFigure{rawFigureSvg=loadFigure figureFile, ..}
			| k == "formula", [(FixArg, [TeXRaw rawFormulaAbbr])] <- a = RawFormula{rawFormulaContent = stuff, ..}
			| isTable e
			, ((_, cap) : (_, [TeXRaw abbr]) : (_, y) : _) <- a
				= RawTable
				{ rawTableCaption = cap
				, rawColumnSpec = parseColspec y
				, rawTableAbbr = "tab:" ++ abbr
				, rawTableBody = breakMultiCols $ parseTable stuff }
			| isTable e = error $ "other table: " ++ show e
			| isTabbing e = RawTabbing stuff
			| isBnf e = RawBnf (if "nc" `isPrefixOf` k then drop 2 k else k) stuff
			| Just ek <- isEnumerate e = RawEnumerated ek (parseItems stuff)
			| isCodeblock e = RawCodeblock e
			| k `elem` ["note", "defnote", "tailnote"] =
			    let label = case a of [(FixArg, [TeXRaw x])] -> x; _ -> "Note"
			    in RawNote label $ parsePara stuff
			| k `elem` ["example", "tailexample"] = RawExample $ parsePara stuff
			| k == "itemdecl" || k == "minipage" || k == "indexeditemdecl" = RawLatexElement e
			| k == "itemdescr" = RawItemdescr $ parsePara stuff
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

instance ExtractFootnotes LaTeX where
	extractFootnotes [] = ([], [])
	extractFootnotes (TeXRaw x : t@(TeXEnv "footnote" _ _ : _))
		= (TeXRaw (Text.stripEnd x) : t', ft)
		where (t', ft) = extractFootnotes t
			-- stripEnd here implements the footnote's \unskip
	extractFootnotes (h:t) = (h' : t', fh ++ ft)
		where
			(h', fh) = extractFootnotes h
			(t', ft) = extractFootnotes t

instance ExtractFootnotes LaTeXUnit where
	extractFootnotes (TeXEnv "footnote" [] content) =
		(TeXComm "footnoteref" "" [], [RawFootnote $ parsePara content])
	extractFootnotes (TeXComm "footnotemark" _ []) =
		(TeXComm "footnoteref" "" [], [])
	extractFootnotes (TeXComm "footnotetext" _ [(_, content)]) =
		(TeXRaw "" {- todo.. -}, [RawFootnote $ parsePara content])
	extractFootnotes (TeXComm a ws [(FixArg, content)]) =
		first (\c -> TeXComm a ws [(FixArg, c)]) (extractFootnotes content)
	extractFootnotes (TeXEnv env args content) = first (TeXEnv env args) (extractFootnotes content)
	extractFootnotes other = (other, [])

parseParas :: LaTeX -> ([RawParagraph], [RawFootnote], LaTeX {- rest -})
parseParas (break isParasEnd -> (extractFootnotes -> (stuff, fs), rest))
		= (collectParas stuff, fs, rest)
	where
		collectParas :: LaTeX -> [RawParagraph]
		collectParas (t@(TeXEnv "indexeditemdecl" _ _) : more) =
			RawParagraph False False (parsePara [t]) Nothing : collectParas more
		collectParas (t@(TeXEnv "itemdecl" _ _) : more) =
			RawParagraph False False (parsePara [t]) Nothing : collectParas more
		collectParas (TeXEnv "itemdescr" _ desc : more) =
			map (\p -> p{rawParaInItemdescr=True}) (collectParas desc)
			++ collectParas more
		collectParas (TeXComm "pnum" _
			[ (FixArg, [TeXRaw (Text.unpack -> file)])
			, (FixArg, [TeXRaw (Text.unpack -> read -> lineNr)])] : more) =
				(\(p : x) -> p{paraNumbered=True, rawParaSourceLoc=Just (SourceLocation file lineNr)} : x)
				(collectParas more)
		collectParas (TeXComm "pnum" _ [] : more) =
				(\(p : x) -> p{paraNumbered=True, rawParaSourceLoc=Nothing} : x)
				(collectParas more)
		collectParas [] = []
		collectParas x = (if null p then id else (RawParagraph False False p Nothing :)) (collectParas more)
			where (parsePara -> p, more) = break isParaEnd x

parseSections :: Int -> LaTeX -> [LinearSection]
parseSections level (TeXComm "textlarger" _ _ : more) = parseSections level more
parseSections level
	(TeXComm c _ args : (parseParas -> (lsectionParagraphs, lsectionFootnotes, more)))
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
parseTable latex
	| triml latex == [] = []
	| triml row == [] = parseTable $ tail rest
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
columnBreakCell = Cell Normal [RawTexPara [RawLatexElement (TeXComm "columnbreak" "" [])]]
isColumnBreakCell :: Cell [RawTexPara] -> Bool
isColumnBreakCell (Cell Normal [RawTexPara [RawLatexElement (TeXComm "columnbreak" _ [])]]) = True
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
                f c@Cell{..} | [RawTexPara (RawLatexElement (TeXComm "columnbreak" _ []) : rest)] <- content =
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

parseWidth :: LaTeX -> (Maybe Text, LaTeX)
parseWidth (TeXRaw "" : x) = parseWidth x
parseWidth (TeXBraces [TeXRaw x] : rest) = (Just x, rest)
parseWidth (TeXBraces [TeXRaw x, TeXComm "hsize" "" []] : rest) =
	(Just $ Text.pack (show (round ((read ("0" ++ Text.unpack x) :: Double) * 100) :: Int)) ++ "%", rest)
parseWidth (TeXBraces _ : rest) = (Nothing, rest) -- remaining cases unsupported for now
parseWidth x = (Nothing, x)

parseColspec :: LaTeX -> [ColumnSpec]
parseColspec = \x -> case x of
		[] -> []
		TeXRaw (Text.unpack -> '|' : z) : y -> go (TeXRaw (Text.pack z) : y)
		_ -> go x
	where
		go :: LaTeX -> [ColumnSpec]
		go [] = []
		go [TeXRaw "|"] = []
		go (TeXRaw "@" : TeXBraces _ : x) = go x -- unimplemented
		go (TeXRaw ">" : TeXBraces _ : x) = go x -- unimplemented
		go (TeXRaw "" : y) = go y
		go (TeXRaw (Text.uncons -> Just (letter, rest)) : y)
		    | letter == ' ' = go (TeXRaw rest : y)
		    | letter == '|' = mapHead (\(ColumnSpec x _ z) -> ColumnSpec x True z) $ go (TeXRaw rest : y)
		    | otherwise =
		    	let (w, rest') = parseWidth (TeXRaw rest : y)
		    	in ColumnSpec (colClass letter) False w : go rest'
		go x = error ("parseColspec: " ++ show x)
		
		colClass :: Char -> TextAlignment
		colClass x | x `elem` ['l', 'm', 'x'] = AlignLeft
		colClass 'p' = Justify
		colClass 'r' = AlignRight
		colClass 'c' = AlignCenter
		colClass other = error $ "Unexpected column type " ++ (other : [])

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
				Cell (Multicolumn (read $ Text.unpack w) (head $ parseColspec cs)) $ parsePara content'
			| otherwise =
				Cell Normal $ parsePara content

rmExplSyntax :: Text -> Text
rmExplSyntax = Text.unlines . f . Text.lines
    where
        f [] = []
        f ("\\ExplSyntaxOn" : (dropWhile (/= "\\ExplSyntaxOff") -> (_ : x))) = f x
        f (h : t) = h : f t

loadMacros :: Text -> IO Macros
loadMacros extraMacros =
	(initialMacros ++)
	. snd
	. doParse initialMacros
	. replace "\\indeximpldef{" "\\index[impldefindex]{"
	. textSubRegex (mkRegex "\\\\penalty[0-9]+{}") ""
	. textSubRegex (mkRegex "\\\\verbtocs{[\\a-zA-Z]*}\\|[^|]*\\|") ""
	. rmExplSyntax
	. (++ extraMacros)
	. mconcat
	. mapM readFile
	["config.tex", "macros.tex", "tables.tex"]

loadXrefDelta :: IO XrefDelta
loadXrefDelta = do
	(tex, _, _) <- Parser.parseString initialContext . Text.unpack . readFile "xrefdelta.tex"
	let lfc c = lookForCommand c tex
	return $
		[ (fromJust $ isJustRaw $ snd from, [snd to])
			| [from, to] <- lfc "movedxrefs" ] ++
		[ (fromJust $ isJustRaw $ snd from, (:[]) . TeXComm "ref" "" . (:[]) . tos)
			| from : tos <- lfc "movedxref" ++ lfc "movedxrefii" ++ lfc "movedxrefiii" ] ++
		[ (abbr, [])
			| [(_, [TeXRaw abbr])] <- lfc "removedxref" ] ++
		[ (abbr, [[TeXComm "ref" "" [(FixArg, [TeXRaw ("depr." ++ abbr)])]]])
			| [(_, [TeXRaw abbr])] <- lfc "deprxref" ]
