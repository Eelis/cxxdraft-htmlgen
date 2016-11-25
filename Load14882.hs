{-# OPTIONS_GHC -fno-warn-tabs #-}
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

module Load14882 (parseIndex, load14882) where

import Document
import Text.LaTeX.Base.Parser
import qualified Text.LaTeX.Base.Render as TeXRender
import Text.LaTeX.Base (protectString)
import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), lookForCommand, matchEnv, matchCommand, (<>))
import Data.Text (Text, replace)
import Data.Text.IO (readFile)
import qualified Data.Text as Text
import Control.Monad (forM)
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (isSpace, isAlpha)
import Control.Arrow (first)
import Data.Map (Map, keys)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Data.List (sort, unfoldr)
import Data.Maybe (isJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Text.Regex (mkRegex, subRegex)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState, evalState, get, put, liftM2)
import Util ((.), (++), mapLast, mapHead)
import LaTeXUtil (texFromArg, mapTeXArg, mapTeXRaw, texTail, concatRaws, mapTeX, Macros(..), Environment(..), Command(..), eval, rmseqs, texStripInfix)

data RawElement
	= RawLatexElements [LaTeX]
	| RawEnumerated String [RawElements]
	| RawBnf String LaTeX
	| RawTable
		{ rawTableCaption :: LaTeX
		, rawColumnSpec :: LaTeX
		, rawTableAbbrs :: [LaTeX]
		, rawTableBody :: [Row RawElements] }
	| RawTabbing LaTeX
	| RawFigure { rawFigureName :: LaTeX, rawFigureAbbr :: LaTeX, rawFigureSvg :: Text }
	| RawFootnote RawElements
	| RawCodeblock LaTeX
	| RawMinipage RawElements
	deriving Show

assignItemNumbers :: Paragraph -> Paragraph
assignItemNumbers p
	| Just n <- paraNumber p = p{ paraElems = fst $ goElems [n,1] (paraElems p) }
	| otherwise = p
	where
		goElems :: [Int] -> [Element] -> ([Element], [Int])
		goElems nn [] = ([], nn)
		goElems nn (e:ee) = case e of
			Enumerated{enumCmd="itemize",..} ->
				let
					items' = map (\(i, Item{..}) ->
						Item
							(Just (mapLast (+i) nn))
							(fst (goElems (mapLast (+i) nn ++ [1]) itemContent))
						) (zip [0..] enumItems)
				in
					first (Enumerated "itemize" items' :) (goElems (mapLast (+ length enumItems) nn) ee)
			_ -> first (e:) (goElems nn ee)

-- We don't represent examples as elements with nested content
-- because sometimes they span multiple (numbered) paragraphs.

type RawElements = [RawElement]

data RawParagraph = RawParagraph
	{ paraNumbered :: Bool
	, rawParaInItemdescr :: Bool
	, rawParaElems :: RawElements }
	deriving Show

data LinearSection = LinearSection
	{ lsectionAbbreviation :: LaTeX
	, lsectionKind :: SectionKind
	, lsectionName :: LaTeX
	, lsectionParagraphs :: [RawParagraph] }
	deriving Show

isEnumerate :: LaTeX -> Maybe String
isEnumerate (TeXEnv s _ _)
	| s `elem` ["enumeratea", "enumerate", "itemize", "description"] = Just s
isEnumerate _ = Nothing

bnfEnvs :: [String]
bnfEnvs = ["bnf", "ncbnf", "bnfkeywordtab", "bnftab", "ncbnftab", "simplebnf", "ncsimplebnf"]

isBnf :: LaTeX -> Bool
isBnf (TeXEnv s _ _)
	| s `elem` bnfEnvs = True
isBnf _ = False

isTable :: LaTeX -> Bool
isTable (TeXEnv "TableBase" _ _) = True
isTable _ = False

isTabbing :: LaTeX -> Bool
isTabbing (TeXEnv "tabbing" _ _) = True
isTabbing _ = False

isFigure :: LaTeX -> Bool
isFigure (TeXEnv "importgraphic" _ _) = True
isFigure _ = False

isCodeblock :: LaTeX -> Bool
isCodeblock (TeXEnv "codeblock" _ _) = True
isCodeblock (TeXEnv "codeblockdigitsep" _ _) = True
isCodeblock _ = False

isMinipage :: LaTeX -> Bool
isMinipage (TeXEnv "minipage" _ _) = True
isMinipage _ = False

isComment :: LaTeX -> Bool
isComment (TeXComment _) = True
isComment _ = False

isParaEnd :: LaTeX -> Bool
isParaEnd (TeXEnv "itemdescr" _ _) = True
isParaEnd (TeXCommS "pnum") = True
isParaEnd x = isParasEnd x

isParasEnd :: LaTeX -> Bool
isParasEnd (TeXComm "definition" _) = True
isParasEnd (TeXComm "definitionx" _) = True
isParasEnd (TeXComm "rSec" _) = True
isParasEnd (TeXComm "infannex" _) = True
isParasEnd (TeXComm "normannex" _) = True
isParasEnd _ = False

isJunk :: LaTeX -> Bool
isJunk (TeXRaw x) = all isSpace (Text.unpack x)
isJunk (TeXComm "index" _) = True
isJunk (TeXComment _) = True
isJunk _ = False

isItem :: LaTeX -> Bool
isItem (TeXCommS "item") = True
isItem (TeXComm "item" _) = True
isItem _ = False

parseItems :: [LaTeX] -> [RawElements]
parseItems [] = []
parseItems (x : (span isJunk -> (junk, rest)))
	| isJunk x = mapHead (RawLatexElements (x : junk) :) (parseItems rest)
parseItems (x : (break isItem -> (item, rest)))
	| isItem x = parsePara item : parseItems rest
parseItems _ = error "need items or nothing"

isElementsEnd :: LaTeX -> Bool
isElementsEnd l =
	isEnumerate l /= Nothing || isBnf l || isTable l
	|| isTabbing l || isFigure l || isCodeblock l || isMinipage l

isTableEnv :: String -> Bool
isTableEnv = (`elem` ["tabular", "longtable"])

texBreak :: (LaTeX -> Bool) -> LaTeX -> (LaTeX, LaTeX)
texBreak p t@(TeXSeq x y)
	| p x = (TeXEmpty, t)
	| (a, b) <- texBreak p x, b /= TeXEmpty = (a, TeXSeq b y)
	| otherwise = first (TeXSeq x) (texBreak p y)
texBreak p t
	| p t = (TeXEmpty, t)
	| otherwise = (t, TeXEmpty)

rowHas :: (String -> Bool) -> LaTeX -> Bool
rowHas f = not . null . matchCommand f

parseTable :: LaTeX -> [Row RawElements]
parseTable TeXEmpty = []
parseTable latex@(TeXSeq _ _)
	| TeXEmpty <- row = parseTable $ texTail rest
	| rowHas (== "endfirsthead") row = parseTable $ findEndHead rest
	| rowHas (`elem` ["caption", "bottomline"]) row = parseTable rest
	| otherwise = makeRow row : parseTable rest
	where
		breakRow = texBreak isRowEnd
		(row, rest) = breakRow latex
		isRowEnd (TeXLineBreak _ _) = True
		isRowEnd _ = False

		findEndHead TeXEmpty = error "Table ended before end of head"
		findEndHead l
			| TeXEmpty <- row' = findEndHead $ texTail rest'
			| rowHas (== "endhead") row' = l
			| otherwise = findEndHead rest'
			where
				(row', rest') = breakRow l

parseTable latex = [makeRow latex]

makeRow :: LaTeX -> Row RawElements
makeRow l = Row sep $ makeRowCells l
	where
		sep
			| rowHas (== "hline") l = RowSep
			| rowHas (== "capsep") l = CapSep
			| rowHas (== "cline") l = Clines $ clines $ lookForCommand "cline" l
			| otherwise = NoSep

		clines [] = []
		clines (([FixArg (TeXRaw c)]) : rest) = (begin, end) : clines rest
			where
				(begin', end') = Text.breakOn "-" c
				begin = read $ Text.unpack begin' :: Int
				end = read $ Text.unpack $ Text.tail end' :: Int
		clines other = error $ "Unexpected \\clines syntax: " ++ show other

makeRowCells :: LaTeX -> [Cell RawElements]
makeRowCells TeXEmpty = []
makeRowCells latex =
	case rest of
		TeXEmpty -> [makeCell cell]
		TeXSeq _ r ->
			(makeCell $ cell <> (TeXRaw cell')) : makeRowCells (TeXSeq (TeXRaw rest'') r)
		TeXRaw r ->
			[(makeCell $ cell <> (TeXRaw cell')), makeCell $ TeXRaw (rest'' ++ r)]
		_ -> error $ "makeRowCells: unexpected " ++ show rest
	where
		(cell, rest) = texBreak isColEnd latex
		isColEnd (TeXRaw c) = isJust $ Text.find (== '&') c
		isColEnd _ = False

		(cell', rest') = Text.break (== '&') $ getText rest
		rest'' = Text.drop 1 rest'
		getText (TeXRaw s) = s
		getText (TeXSeq (TeXRaw s) _) = s
		getText other = error $ "Didn't expect " ++ show other

		getContent = parsePara . rmseqs

		makeCell content
			| [[FixArg (TeXRaw w), FixArg cs, FixArg content']] <- lookForCommand "multicolumn" content =
				Cell (Multicolumn (read $ Text.unpack w) cs) $ getContent content'
			| otherwise =
				Cell Normal $ getContent content

loadFigure :: Text -> Text
loadFigure f =
		rmIds $ snd $ Text.breakOn "<svg" $ Text.pack
			$ unsafePerformIO (readProcess "dot" ["-Tsvg", "-Gbgcolor=transparent", p] "")
	where
		p = Text.unpack $ Text.replace ".pdf" ".dot" f
		r = mkRegex "<g id=\"[^\"]*\"" 
		rmIds = Text.pack . flip (subRegex r) "<g" . Text.unpack
			-- Without rmIds, if a page has more than one figure, it will
			-- have duplicate 'graph1', 'node1', 'edge1' etc ids.

class ExtractFootnotes a where extractFootnotes :: a -> (a, [RawElements])

instance (Monoid a, ExtractFootnotes a) => ExtractFootnotes [a] where
	extractFootnotes l = (map fst x, x >>= snd)
		where x = extractFootnotes . l

instance ExtractFootnotes LaTeX where
	extractFootnotes (TeXComm "footnote" [FixArg content]) =
		(TeXCommS "footnoteref", [parsePara $ rmseqs content])
	extractFootnotes (TeXCommS "footnotemark") =
		(TeXCommS "footnoteref", [])
	extractFootnotes (TeXComm "footnotetext" [FixArg content]) =
		(TeXEmpty, [parsePara $ rmseqs content])
	extractFootnotes (TeXComm a [FixArg content]) =
		first (\c -> TeXComm a [FixArg c]) (extractFootnotes content)
	extractFootnotes (TeXSeq a b) = extractFootnotes a ++ extractFootnotes b
	extractFootnotes (TeXEnv env args content) = first (TeXEnv env args) (extractFootnotes content)
	extractFootnotes other = (other, [])

parsePara :: [LaTeX] -> RawElements
parsePara [] = []
parsePara (env@(TeXEnv _ _ _) : more) =
	go e' : parsePara more ++ (RawFootnote . footnotes)
	where
		go :: LaTeX -> RawElement
		go e@(TeXEnv k a stuff)
			| isFigure e
			, [FixArg rawFigureName, FixArg rawFigureAbbr, FixArg (TeXRaw figureFile)] <- a
			= RawFigure{rawFigureSvg=loadFigure figureFile, ..}
			| isTable e
			, ((x : _todo) : _) <- lookForCommand "caption" stuff
			, (_, (FixArg y : _), content) : _todo <- matchEnv isTableEnv stuff
			= RawTable
				{ rawTableCaption = texFromArg x
				, rawColumnSpec = y
				, rawTableAbbrs = map (texFromArg . head) (lookForCommand "label" stuff)
				, rawTableBody = parseTable content }
			| isTable e = error $ "other table: " ++ show e
			| isTabbing e = RawTabbing stuff
			| isCodeblock e = RawCodeblock stuff
			| isBnf e = RawBnf k stuff
			| isMinipage e = RawMinipage $ parsePara $ rmseqs stuff
			| Just ek <- isEnumerate e = RawEnumerated ek (parseItems $ rmseqs stuff)
		go other = error $ "parsePara: unexpected " ++ show other

		(e', footnotes) = extractFootnotes env

parsePara (elems -> (extractFootnotes -> (e, footnotes), more))
	= RawLatexElements e : parsePara more ++ (RawFootnote . footnotes)

elems :: [LaTeX] -> ([LaTeX], [LaTeX])
elems [] = ([], [])
elems y@(x:xs)
	| isElementsEnd x = ([], y)
	| TeXRaw (Text.breakOn "\n\n" -> (a, b)) <- x
	, b /= "" = ([TeXRaw a], TeXRaw (Text.drop 2 b) : xs)
	| otherwise = first (x :) (elems xs)

parseParas :: [LaTeX] -> ([RawParagraph], [LaTeX] {- rest -})
parseParas (break isParasEnd -> (extractFootnotes -> (stuff, fs), rest))
		= (collectParas stuff ++ [RawParagraph False False $ RawFootnote . fs], rest)
	where
		collectParas :: [LaTeX] -> [RawParagraph]
		collectParas (TeXEnv "itemdescr" _ desc : more) =
			map (\p -> p{rawParaInItemdescr=True}) (collectParas $ rmseqs desc)
			++ collectParas more
		collectParas (TeXCommS "pnum" : more) =
			(\(p : x) -> p{paraNumbered=True} : x) (collectParas more)
		collectParas [] = []
		collectParas x = (RawParagraph False False (parsePara p) : ps)
			where
				ps = collectParas more
				(p, more) = break isParaEnd x

parseSections :: [LaTeX] -> [LinearSection]
parseSections
	(TeXComm c args
	: ( parseParas ->
		( lsectionParagraphs
		, parseSections -> moreSections
	)))
	| (lsectionAbbreviation, lsectionName, lsectionKind) <- case (c, args) of
		("normannex", [FixArg abbr, FixArg name]) ->
			(abbr, name, NormativeAnnexSection)
		("infannex", [FixArg abbr, FixArg name]) ->
			(abbr, name, InformativeAnnexSection)
		("rSec", [OptArg (TeXRaw level), OptArg abbr, FixArg name]) ->
			(abbr, name, NormalSection $ read $ Text.unpack level)
		("definition", [FixArg name, FixArg abbr]) ->
			(abbr, name, DefinitionSection 2)
		("definitionx", [FixArg name, FixArg abbr]) ->
			(abbr, name, DefinitionSection 3)
		_ -> error $ "not a section command: " ++ show c
	= LinearSection{..} : moreSections
parseSections [] = []
parseSections (x:_) = error $ "parseSections: " ++ show x

translateVerb :: String -> String
translateVerb ('\\':'v':'e':'r':'b':delim:rest) =
	"\\verb{" ++ (protectString inside) ++ "}" ++ translateVerb rest'
	where
		(inside, _ : rest') = break (== delim) rest
translateVerb (x:y) = x : translateVerb y
translateVerb [] = []

initialMacros :: Macros
initialMacros = mempty
	{ environments = Map.fromList
		[ ("ttfamily", Environment mempty mempty [])
		, ("paras",    Environment mempty mempty []) ]
	, commands = Map.fromList
		[ ("gramSec", Command 2 "") ]}

dontEval :: [Text]
dontEval = map Text.pack $ bnfEnvs ++ words "drawing definition definitionx Cpp importgraphic bottomline capsep bigoh itemdescr grammarterm nontermdef defnx FlushAndPrintGrammar term caret indented enumeratea"

parseStr :: String -> LaTeX
parseStr = doParse . Text.pack

reparseTabs :: LaTeX -> LaTeX
reparseTabs = doParse . Text.replace "\\>" "\t" . TeXRender.render

stringLiteral :: String -> (String, String)
stringLiteral ('\\' : '"' : x) = first ("\\\"" ++) (stringLiteral x)
stringLiteral ('\\' : '\\' : x) = first ("\\\\" ++) (stringLiteral x)
stringLiteral ('"' : x) = ("\"", x)
stringLiteral (c : x) = first (c :) (stringLiteral x)
stringLiteral "" = ("", "")

reparseCode :: LaTeX -> LaTeX
reparseCode t = parse False . Text.unpack $ TeXRender.render t
	where
		parse :: Bool {- in string literal -} -> String -> LaTeX
		parse _ "" = TeXEmpty
		parse b ('@' : rest) = parseStr cmd <> parse b rest'
			where (cmd, '@' : rest') = break (== '@') rest
		parse True ('"' : rest) = "\"" <> parse False rest
		parse False ('"' : rest) = "\"" <> parse True lit <> parse False rest'
			where (lit, rest') = stringLiteral rest
		parse False ('/' : '/' : rest) = TeXComm "comment" [FixArg (parseStr $ "//" ++ comment)] <> parse False rest'
			where (comment, rest') = breakLineComment rest
		parse False ('/' : '*' : rest) = TeXComm "comment" [FixArg ("/*" <> parse False comment)] <> parse False rest'
			where (comment, rest') = breakComment rest
		parse b ('/' : rest) = "/" <> parse b rest
		parse b s = TeXRaw (Text.pack code) <> parse b rest
			where (code, rest) = break (`elem` ['@', '/', '"']) s

		breakLineComment s = case break (== '\n') s of
			(comment, '\n' : rest) -> (comment ++ "\n", rest)
			(x, y) -> (x, y)

		breakComment s = go s ""
			where
				go "" a = (reverse a, "")
				go ('*' : '/' : rest) a = (reverse $ '/' : '*' : a, rest)
				go (x : rest) a = go rest (x : a)

reparseEnvs :: LaTeX -> LaTeX
reparseEnvs = mapTeX $
	\case
		TeXEnv c [] body | c `elem` ["codeblock", "itemdecl"] ->
			Just $ TeXEnv c [] $ reparseCode body
		TeXEnv t [] body | t `elem` ["bnfkeywordtab", "bnftab", "ncbnftab", "tabbing"] ->
			Just $ TeXEnv t [] $ reparseTabs body
		_ -> Nothing

-- \@. becomes \atDot
-- .\@ becomes \dotAt
reparseAtCommand :: LaTeX -> LaTeX
reparseAtCommand (TeXSeq (TeXRaw b) (TeXSeq (TeXCommS "") (TeXSeq (TeXRaw a) rest))) =
	if Text.head a /= '@' then
		TeXSeq (TeXRaw b) (
			TeXSeq (TeXCommS "") (
				TeXSeq (TeXRaw a) (reparseAtCommand rest)))
	else
		if Text.last b == '.' then
			TeXSeq (TeXRaw $ Text.dropEnd 1 b) (
				TeXSeq (TeXCommS "dotAt") (
					TeXSeq (TeXRaw $ Text.drop 1 a) (reparseAtCommand rest)))
		else
			if Text.index a 1 /= '.' then error("\\@ without dot detected") else
			TeXSeq (TeXRaw b) (
				TeXSeq (TeXCommS "atDot") (
					TeXSeq (TeXRaw $ Text.drop 2 a) (reparseAtCommand rest)))
reparseAtCommand (TeXSeq l r) = TeXSeq (reparseAtCommand l) (reparseAtCommand r)
reparseAtCommand (TeXComm n args) = TeXComm n $ map (mapTeXArg reparseAtCommand) args
reparseAtCommand (TeXEnv n args body) = TeXEnv n (map (mapTeXArg reparseAtCommand) args) (reparseAtCommand body)
reparseAtCommand x = x


moreArgs :: LaTeX -> LaTeX
moreArgs (TeXSeq (TeXComm n a) (TeXSeq (TeXBraces x) more))
	= moreArgs (TeXSeq (TeXComm n (a ++ [FixArg x])) more)
moreArgs (TeXComm n a) = TeXComm n (map (mapTeXArg moreArgs) a)
moreArgs (TeXSeq x y) = moreArgs x ++ moreArgs y
moreArgs (TeXEnv e a x) 
	| otherwise = TeXEnv e (map (mapTeXArg moreArgs) a) (moreArgs x)
moreArgs (TeXBraces x) = TeXBraces (moreArgs x)
moreArgs x = x

doParse :: Text -> LaTeX
doParse t = case parseLaTeX t of
	Left e -> error (show e ++ "\nFor given input: " ++ Text.unpack t)
	Right l -> l

doParseLaTeX :: Text -> LaTeX
doParseLaTeX =
	reparseAtCommand
	. reparseEnvs
	. moreArgs
	. doParse

newlineCurlies :: Text -> Text
newlineCurlies =
	replace "\n{" "{"
	. replace "\n\t{" "{"
	. replace "\n {" "{"
	. replace "}\n  {" "}{"
	. replace "} \n  {" "}{"
		-- Todo: These are sometimes inappropriate...

parseFile :: Macros -> Text -> [LinearSection]
parseFile macros =
	parseSections
	. filter (not . isComment)
	. rmseqs
	. fst . eval macros dontEval
	. doParseLaTeX
	. replace "$$" "$"
	. replace "\\hspace*" "\\hspace"
	. newlineCurlies
	. replace "``" "“"
	. replace "''" "”"
	. replace "\\rSec0" "\\rSec[0]"
	. replace "\\rSec1" "\\rSec[1]"
	. replace "\\rSec2" "\\rSec[2]"
	. replace "\\rSec3" "\\rSec[3]"
	. replace "\\rSec4" "\\rSec[4]"
	. replace "\\rSec5" "\\rSec[5]"
	. replace "\\bigl[" "\\bigl ["
	. Text.pack . translateVerb . Text.unpack

getCommitUrl :: IO Text
getCommitUrl = do
	url <- gitGetRemoteUrl
	commit <- gitGetCommitRef
	return $
		( Text.replace "git@github.com:" "http://github.com/"
		$ Text.replace ".git" "/commit/" url)
		++ commit

gitGetRemoteUrl :: IO Text
gitGetRemoteUrl = do
	x <- readProcess "git" ["ls-remote", "--get-url"] ""
	return $ Text.strip $ Text.pack x

gitGetCommitRef :: IO Text
gitGetCommitRef = do
	x <- readProcess "git" ["rev-parse", "HEAD"] ""
	return $ Text.strip $ Text.pack $ x


-- Numbering

data Numbers = Numbers { tableNr, figureNr, footnoteRefNr, footnoteNr :: Int }

class AssignNumbers a b | a -> b where
	assignNumbers :: forall m . (Functor m, MonadFix m, MonadState Numbers m) => Section -> a -> m b

instance AssignNumbers LaTeX LaTeX where
	assignNumbers s (TeXSeq x y) = liftM2 TeXSeq (assignNumbers s x) (assignNumbers s y)
	assignNumbers s (TeXEnv x y z) = TeXEnv x y . assignNumbers s z
	assignNumbers s (TeXComm x [FixArg y]) = TeXComm x . (:[]) . FixArg . assignNumbers s y
	assignNumbers _ (TeXCommS "footnoteref") = do
		Numbers{..} <- get
		put Numbers{footnoteRefNr = footnoteRefNr+1, ..}
		return $ TeXComm "footnoteref" [FixArg $ TeXRaw $ Text.pack $ show footnoteRefNr]
	assignNumbers _ x = return x

instance AssignNumbers a b => AssignNumbers (Cell a) (Cell b) where
	assignNumbers s x@Cell{..} = do
		content' <- assignNumbers s content
		return x{content=content'}

instance AssignNumbers a b => AssignNumbers (Row a) (Row b) where
	assignNumbers s x@Row{..} = do
		cells' <- assignNumbers s cells
		return x{cells=cells'}

instance AssignNumbers RawElement Element where
	assignNumbers section RawFigure{..} = do
		Numbers{..} <- get
		put Numbers{figureNr = figureNr+1, ..}
		return $ FigureElement Figure
			{ figureNumber  = figureNr
			, figureName    = rawFigureName
			, figureAbbr    = rawFigureAbbr
			, figureSvg     = rawFigureSvg
			, figureSection = section }
	assignNumbers s RawTable{..} = do
		Numbers{..} <- get
		put Numbers{tableNr = tableNr+1, ..}
		tableBody <- assignNumbers s rawTableBody
		return $ TableElement Table
			{ tableNumber  = tableNr
			, columnSpec   = rawColumnSpec
			, tableAbbrs   = rawTableAbbrs
			, tableCaption = rawTableCaption
			, tableSection = s
			, .. }
	assignNumbers s (RawFootnote t) = do
		Numbers{..} <- get
		put Numbers{footnoteNr = footnoteNr+1, ..}
		t' <- assignNumbers s t
		return Footnote{footnoteNumber=footnoteNr,footnoteContent=t'}
	assignNumbers s (RawEnumerated x p) = Enumerated x . (Item Nothing .) . assignNumbers s p
	assignNumbers s (RawLatexElements x) = LatexElements . assignNumbers s x
	assignNumbers _ (RawBnf x y) = return $ Bnf x y
	assignNumbers _ (RawTabbing x) = return $ Tabbing x
	assignNumbers s (RawCodeblock x) = Codeblock . assignNumbers s x
	assignNumbers s (RawMinipage x) = Minipage . assignNumbers s x

lsectionLevel :: LinearSection -> Int
lsectionLevel (lsectionKind -> NormalSection l) = l
lsectionLevel (lsectionKind -> DefinitionSection l) = l
lsectionLevel _ = 0

paraNumbers :: [Bool] -> [Maybe Int]
paraNumbers = f 1
	where
		f _ [] = []
		f i (True : x) = Just i : f (i + 1) x
		f i (False : x) = Nothing : f i x

treeizeChapters :: forall m . (Functor m, MonadFix m, MonadState Numbers m) =>
	Int -> [LinearSection] -> m [Section]
treeizeChapters _ [] = return []
treeizeChapters sectionNumber (LinearSection{..} : more) = mdo
		newSec <- return Section{..}
		let pn = paraNumbers $ paraNumbered . lsectionParagraphs
		paragraphs <- forM (zip pn lsectionParagraphs) $
			\(paraNumber, RawParagraph{..}) -> do
				paraElems <- assignNumbers newSec rawParaElems
				return $ assignItemNumbers $ Paragraph{paraInItemdescr = rawParaInItemdescr, ..}
		subsections <- treeizeSections 1 chapter [newSec] lsubsections
		more'' <- treeizeChapters (sectionNumber + 1) more'
		return $ newSec : more''
	where
		parents = []
		chapter
			| lsectionKind == InformativeAnnexSection = InformativeAnnex
			| lsectionKind == NormativeAnnexSection = NormativeAnnex
			| otherwise = NormalChapter
		abbreviation = lsectionAbbreviation
		sectionName = lsectionName
		(lsubsections, more') = span ((> 0) . lsectionLevel) more

treeizeSections :: forall m . (Functor m, MonadFix m, MonadState Numbers m) =>
	Int -> Chapter -> [Section] -> [LinearSection] -> m [Section]
treeizeSections _ _ _ [] = return []
treeizeSections sectionNumber chapter parents (s@LinearSection{..} : more) = mdo
		newSec <- return Section{..}
		let pn = paraNumbers $ paraNumbered . lsectionParagraphs
		paragraphs <- forM (zip pn lsectionParagraphs) $
			\(paraNumber, RawParagraph{..}) -> do
				paraElems <- assignNumbers newSec rawParaElems
				return $ assignItemNumbers $ Paragraph{paraInItemdescr = rawParaInItemdescr, ..}
		subsections <- treeizeSections 1 chapter (newSec : parents) lsubsections
		more'' <- treeizeSections (sectionNumber + 1) chapter parents more'
		return $ newSec : more''
	where
		abbreviation = lsectionAbbreviation
		sectionName = lsectionName
		n = lsectionLevel s
		(lsubsections, more') = span ((> n) . lsectionLevel) more

instance AssignNumbers a b => AssignNumbers [a] [b] where
	assignNumbers s = mapM (assignNumbers s)

type GrammarLinks = Map Text Section

nontermdefsInSection :: Section -> GrammarLinks
nontermdefsInSection s@Section{..} =
	Map.unions $
	((Map.fromList $ map (, s) (paragraphs >>= paraElems >>= nontermdefsInElement))
	: map nontermdefsInSection subsections)

nontermdefsInElement :: Element -> [Text]
nontermdefsInElement (LatexElements e) = concatMap nontermdefs e
nontermdefsInElement (Bnf _ e) = nontermdefs e
nontermdefsInElement _ = []

nontermdefs :: LaTeX -> [Text]
nontermdefs (TeXSeq a b) = (nontermdefs a) ++ (nontermdefs b)
nontermdefs (TeXEnv _ _ x) = nontermdefs x
nontermdefs (TeXBraces x) = nontermdefs x
nontermdefs (TeXComm "nontermdef" [FixArg (TeXRaw name)]) = [name]
nontermdefs _ = []

resolveGrammarterms :: GrammarLinks -> Section -> Section
resolveGrammarterms links Section{..} =
	Section{
		paragraphs  = map (\p -> p{paraElems = map (resolve links) (paraElems p)}) paragraphs,
		subsections = map (resolveGrammarterms links) subsections,
		..}
	where
		resolve :: GrammarLinks -> Element -> Element
		resolve g (LatexElements e) = LatexElements $ map (grammarterms g) e
		resolve g (Enumerated s ps) = Enumerated s $ map f ps
			where f i@Item{..} = i{itemContent=map (resolve g) itemContent}
		resolve g (Bnf n b) = Bnf n $ bnfGrammarterms g b
		resolve g (Footnote n c) = Footnote n $ map (resolve g) c
		resolve _ other = other

grammarterms :: GrammarLinks -> LaTeX -> LaTeX
grammarterms links = mapTeX (go links)
	where
		go g (TeXComm "grammarterm" args@((FixArg (TeXRaw name)) : _))
			| Just Section{..} <- Map.lookup (Text.toLower name) g =
			Just $ TeXComm "grammarterm_" ((FixArg abbreviation) : args)
		go _ _ = Nothing

bnfGrammarterms :: GrammarLinks -> LaTeX -> LaTeX
bnfGrammarterms links = go links . mapTeX wordify
	where
		wordify :: LaTeX -> Maybe LaTeX
		wordify (TeXRaw stuff) = Just $ mconcat $ map TeXRaw $ unfoldr f stuff
			where
				f s | Text.null s = Nothing
				f s | isName $ Text.head s = Just $ Text.span isName s
				f s = Just $ Text.break isName s

				isName c = isAlpha c || c `elem` ['-', '_']
		wordify _ = Nothing

		go g n@(TeXRaw name)
			| Just Section{..} <- Map.lookup name g =
				TeXComm "grammarterm_" [(FixArg abbreviation), (FixArg n)]
		go g (TeXSeq a b) = TeXSeq (go g a) (go g b)
		go _ other = other

parseIndex :: LaTeX -> (IndexPath, Maybe IndexKind)
parseIndex = go . mapTeXRaw unescapeIndexPath . concatRaws

	where
		go (texStripInfix "|seealso" -> Just (x, y)) = (parseIndexPath x, Just $ See True y)
		go (texStripInfix "|see" -> Just (x, y)) = (parseIndexPath x, Just $ See False y)
		go (texStripInfix "|(" -> Just (t, _)) = (parseIndexPath t, Just IndexOpen)
		go (texStripInfix "|)" -> Just (t, _)) = (parseIndexPath t, Just IndexClose)
		go t = (parseIndexPath t, Nothing)

		unescapeIndexPath :: Text -> LaTeX
		unescapeIndexPath = TeXRaw
			. replace "\5" "\""

			. replace "\2" "!"
			. replace "!" "\1"
			. replace "\"!" "\2"

			. replace "\4" "@"
			. replace "@" "\3"
			. replace "\"@" "\4"

			. replace "\"|" "|"
			. replace "\"\"" "\5"

		parseIndexPath :: LaTeX -> IndexPath
		parseIndexPath (texStripInfix "\1" -> Just (x, y)) = parseIndexPath x ++ parseIndexPath y
		parseIndexPath (texStripInfix "\3" -> Just (x, y)) = [IndexComponent x y]
		parseIndexPath t = [IndexComponent t TeXEmpty]

data RawIndexEntry = RawIndexEntry
	{ indexSection :: Section
	, indexCategory :: Text
	, rawIndexPath :: IndexPath
	, rawIndexKind :: Maybe IndexKind }

instance Show RawIndexEntry where
	show RawIndexEntry{..} =
		"RawIndexEntry"
		++ "{indexSection=" ++ show (sectionName indexSection)
		++ ",indexCategory=" ++ show indexCategory
		++ ",rawIndexPath=" ++ show rawIndexPath
		++ "}"

sectionIndexEntries :: Section -> [RawIndexEntry]
sectionIndexEntries s =
	[ RawIndexEntry{..}
	| indexSection <- withSubsections s
	, [OptArg (TeXRaw indexCategory), FixArg (parseIndex -> (rawIndexPath, rawIndexKind))]
		<- paragraphs indexSection >>= paraElems >>= elemTex >>= lookForCommand "index" ] ++
	[ RawIndexEntry
		{ indexCategory = "generalindex"
		, rawIndexKind = Just DefinitionIndex
		, ..}
	| indexSection <- withSubsections s
	, [FixArg _, FixArg (parseIndex -> (rawIndexPath, Nothing))]
		<- paragraphs indexSection >>= paraElems >>= elemTex >>= lookForCommand "defnx" ]

toIndex :: RawIndexEntry -> Index
toIndex RawIndexEntry{..} = Map.singleton indexCategory $ go rawIndexPath
	where
		go :: [IndexComponent] -> IndexTree
		go [c] = Map.singleton c (IndexNode [IndexEntry indexSection rawIndexKind rawIndexPath] Map.empty)
		go (c:cs) = Map.singleton c $ IndexNode [] $ go cs
		go _ = error "toIndex"

load14882 :: IO Draft
load14882 = do

	commitUrl <- getCommitUrl

	macros@Macros{..} <-
		(initialMacros ++)
		. snd . eval mempty dontEval
		. doParseLaTeX
		. replace "\\indeximpldef{" "\\index[impldefindex]{"
		. newlineCurlies
		. mconcat
		. mapM readFile
		["config.tex", "macros.tex", "tables.tex"]

	putStrLn $ ("Loaded macros: " ++) $ unwords $ sort $
		keys commands ++ (Text.unpack . keys environments)

	let
		files :: [FilePath]
		files = words $
			"intro lex basic conversions expressions statements " ++
			"declarations declarators classes derived access special " ++
			"overloading templates exceptions preprocessor lib-intro " ++
			"support diagnostics utilities strings locales containers " ++
			"iterators algorithms numerics iostreams regex atomics threads " ++
			"grammar limits compatibility future"

	putStrLn "Loading chapters"
	secs <- forM files $ \c -> do
		let p = c ++ ".tex"
		putStr $ "  " ++ c ++ "... "; hFlush stdout

		stuff <-
			replace "\\indeximpldef{" "\\index[impldefindex]{" .
			readFile p

		extra <-
			if c /= "grammar" then return ""
			else replace "\\gramSec" "\\rSec[1]" . readFile "std-gram.ext"

		let r = parseFile macros (stuff ++ extra)

		putStrLn $ show (length r) ++ " sections"
		return r

	if length (show secs) == 0 then undefined else do
		-- force eval before we leave the dir
		let chapters = evalState (treeizeChapters 1 $ mconcat secs) (Numbers 1 1 1 1)

		let ntdefs = Map.unions $ map nontermdefsInSection chapters
		let chapters' = map (resolveGrammarterms ntdefs) chapters
		let index = mergeIndices $ map toIndex (chapters' >>= sectionIndexEntries)

		return Draft{chapters=chapters', ..}
