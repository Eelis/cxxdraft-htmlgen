{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	ViewPatterns,
	LambdaCase,
	TupleSections,
	NamedFieldPuns,
	TypeSynonymInstances,
	FlexibleInstances,
	FlexibleContexts,
	RankNTypes,
	MultiParamTypeClasses,
	FunctionalDependencies,
	UndecidableInstances,
	RecursiveDo #-}

module Load14882 (
	CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Elements, Paragraph(..),
	Section(..), Chapter(..), Draft(..), Table(..), Figure(..),
	IndexPath, IndexComponent(..), IndexCategory, Index, IndexTree, IndexNode(..), IndexEntry(..), IndexKind(..),
	parseIndex, indexKeyContent, indexCatName, sections,
	coreChapters, libChapters, figures, tables, tableByAbbr, figureByAbbr,
	LaTeX,
	load14882) where

import Text.LaTeX.Base.Parser
import qualified Text.LaTeX.Base.Render as TeXRender
import Text.LaTeX.Base (protectString)
import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), lookForCommand, matchEnv, matchCommand, (<>), texmap, MathType(Dollar))
import Data.Text (Text, replace)
import Data.Text.IO (readFile)
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Function (on)
import Control.Monad (forM)
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (isSpace, ord, isDigit, isAlpha, isAlphaNum, toLower)
import Control.Arrow (first)
import Data.Map (Map, keys, lookup)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Data.List (sort, unfoldr, stripPrefix)
import Data.Maybe (isJust, fromJust, listToMaybe)
import Data.String (IsString)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Text.Regex (mkRegex, subRegex)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState, evalState, get, put, liftM2)
import Util (greekAlphabet)

(++) :: Monoid a => a -> a -> a
(++) = mappend

data CellSpan = Normal | Multicolumn { width :: Int, colspec :: LaTeX } deriving Show
data Cell a = Cell { cellSpan :: CellSpan, content :: a } deriving Show
data RowSepKind = RowSep | CapSep | Clines [(Int, Int)] | NoSep deriving Show
data Row a = Row { rowSep :: RowSepKind, cells :: [Cell a] } deriving Show

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

data Table = Table
	{ tableNumber :: Int
	, tableCaption :: LaTeX
	, columnSpec :: LaTeX
	, tableAbbrs :: [LaTeX]
	, tableBody :: [Row Elements]
	, tableSection :: Section }
	deriving Show

data Figure = Figure
	{ figureNumber :: Int
	, figureName :: LaTeX
	, figureAbbr :: LaTeX
	, figureSvg :: Text
	, figureSection :: Section }
	deriving Show

data Element
	= LatexElements [LaTeX]
	| Enumerated String [Elements]
	| Bnf String LaTeX
	| TableElement Table
	| Tabbing LaTeX
	| FigureElement Figure
	| Footnote { footnoteNumber :: Int, footnoteContent :: Elements }
	| Codeblock { code :: LaTeX }
	| Minipage Elements
	deriving Show

-- We don't represent examples as elements with nested content
-- because sometimes they span multiple (numbered) paragraphs.

type RawElements = [RawElement]
type Elements = [Element]

data RawParagraph = RawParagraph
	{ paraNumbered :: Bool
	, rawParaInItemdescr :: Bool
	, rawParaElems :: RawElements }
	deriving Show

data SectionKind
	= NormalSection { _level :: Int }
	| DefinitionSection { _level :: Int }
	| InformativeAnnexSection
	| NormativeAnnexSection
	deriving (Eq, Show)

data Chapter = NormalChapter | InformativeAnnex | NormativeAnnex
	deriving (Eq, Show)

data LinearSection = LinearSection
	{ lsectionAbbreviation :: LaTeX
	, lsectionKind :: SectionKind
	, lsectionName :: LaTeX
	, lsectionParagraphs :: [RawParagraph] }
	deriving Show

data Paragraph = Paragraph
	{ paraNumber :: Maybe Int
	, paraInItemdescr :: Bool
	, paraElems :: Elements }
	deriving Show

data Section = Section
	{ abbreviation :: LaTeX
	, sectionName :: LaTeX
	, paragraphs :: [Paragraph]
	, subsections :: [Section]
	, sectionNumber :: Int
	, chapter :: Chapter
	, parents :: [Section] -- if empty, this is the chapter
	}
	deriving Show

instance Eq Section where
	x == y = abbreviation x == abbreviation y

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

rmseqs :: LaTeX -> [LaTeX]
rmseqs (TeXSeq x y) = rmseqs x ++ rmseqs y
rmseqs x = [x]

class Sections a where sections :: a -> [Section]

instance Sections Section where sections s = s : concatMap sections (subsections s)

isEnumerate :: LaTeX -> Maybe String
isEnumerate (TeXEnv s _ _)
	| s `elem` ["enumeraten", "enumeratea", "enumerate", "itemize", "description"] = Just s
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
	-- Todo: render the different kinds of items properly

type Item = RawElements

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:y) = f x : y
mapHead _ [] = []

parseItems :: [LaTeX] -> [Item]
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

texTail :: LaTeX -> LaTeX
texTail (TeXSeq _ t) = t
texTail _ = error "Not a sequence"

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

data Command = Command
	{ arity :: !Int
	, body :: !LaTeX }
	deriving Show

data Environment = Environment
	{ begin, end :: !LaTeX
	, defaultArgs :: ![LaTeX] }
	deriving Show

mapTeXArg :: (LaTeX -> LaTeX) -> (TeXArg -> TeXArg)
mapTeXArg f (FixArg t) = FixArg (f t)
mapTeXArg f (OptArg t) = OptArg (f t)
mapTeXArg _ x = x

texFromArg :: TeXArg -> LaTeX
texFromArg (FixArg t) = t
texFromArg (OptArg t) = t
texFromArg (SymArg t) = t
texFromArg _ = error "no"

mapTeXRaw :: (Text -> LaTeX) -> (LaTeX -> LaTeX)
mapTeXRaw f = go
	where
		go :: LaTeX -> LaTeX
		go (TeXRaw t) = f t
		go (TeXComm s args) = TeXComm s (mapTeXArg go . args)
		go (TeXEnv s args body) = TeXEnv s (mapTeXArg go . args) (go body)
		go (TeXBraces l) = TeXBraces $ go l
		go TeXEmpty = TeXEmpty
		go (TeXSeq x y) = TeXSeq (go x) (go y)
		go t@(TeXCommS _) = t
		go t@(TeXComment _) = t
		go t@(TeXMath _ _) = t
		go t@(TeXLineBreak _ _) = t

extractText :: LaTeX -> Text
extractText (TeXRaw s) = s
extractText (TeXSeq a b) = (extractText a) ++ (extractText b)
extractText TeXEmpty = ""
extractText _ = error "extractText"

data Macros = Macros
	{ commands :: Map String Command
	, environments :: Map Text Environment
	, counters :: Map Text Int }
	deriving Show

initialMacros :: Macros
initialMacros = mempty
	{ environments = Map.fromList
		[ ("ttfamily", Environment mempty mempty [])
		, ("paras",    Environment mempty mempty []) ]
	, commands = Map.fromList
		[ ("gramSec", Command 2 "") ]}

instance Monoid Macros where
	mempty = Macros mempty mempty mempty
	mappend x y = Macros (commands x ++ commands y) (environments x ++ environments y) (counters x ++ counters y)

getDigit :: Char -> Maybe Int
getDigit c
	| isDigit c = Just $ ord c - ord '0'
	| otherwise = Nothing

replArgs :: [LaTeX] -> LaTeX -> LaTeX
replArgs args = mapTeXRaw (replaceArgsInString args . Text.unpack)

ppp :: Text -> (LaTeX -> LaTeX)
ppp "" = id
ppp t = TeXSeq (TeXRaw t)

concatRaws :: LaTeX -> LaTeX
concatRaws l =
		if b == ""
			then a
			else (if a == TeXEmpty then id else TeXSeq a) (TeXRaw b)
	where
		go :: Text -> LaTeX -> (LaTeX, Text)
		go pre t@TeXEmpty = (t, pre)
		go pre (TeXRaw s) = (TeXEmpty, pre ++ s)
		go pre (TeXEnv s args body) = (ppp pre $ TeXEnv s (mapTeXArg concatRaws . args) (concatRaws body), "")
		go pre (TeXComm s args) = (ppp pre $ TeXComm s (mapTeXArg concatRaws . args), "")
		go pre (TeXCommS s) = (ppp pre $ TeXCommS s, "")
		go pre (TeXBraces x) = (ppp pre $ TeXBraces (concatRaws x), "")
		go pre (TeXComment t) = (ppp pre $ TeXComment t, "")
		go pre (TeXMath m t) = (ppp pre $ TeXMath m t, "")
		go pre t@(TeXLineBreak _ _) = (ppp pre t, "")
		go pre (TeXSeq x y) =
			let
				(x', s) = go pre x
				(y', s') = go s y
			in
				((if x' /= TeXEmpty then TeXSeq x' else id) y', s')
		(a, b) =  go "" l

replaceArgsInString :: [LaTeX] -> String -> LaTeX
replaceArgsInString args = concatRaws . go
	where
		go :: String -> LaTeX
		go ('#':'#':more) = TeXRaw "#" ++ go more
		go ('#':c:more)
			| Just i <- getDigit c =
			(args !! (i-1)) ++
			go more
		go (c : more) = TeXRaw (Text.pack [c]) ++ go more
		go [] = TeXEmpty

dontEval :: [Text]
dontEval = map Text.pack $ bnfEnvs ++ words "drawing definition definitionx Cpp importgraphic bottomline capsep bigoh itemdescr grammarterm nontermdef defnx FlushAndPrintGrammar"

recognizeEnvs :: LaTeX -> LaTeX
recognizeEnvs (TeXSeq b@(TeXComm "begin" (FixArg (TeXRaw n) : aa)) rest) =
		case findEnd n rest of
			Nothing -> TeXSeq b (recognizeEnvs rest)
			Just (x, y) -> TeXSeq (TeXEnv (Text.unpack n) aa (recognizeEnvs x)) (recognizeEnvs y)
	where
		findEnd :: Text -> LaTeX -> Maybe (LaTeX, LaTeX)
		findEnd n' (TeXSeq (TeXComm "end" [FixArg (TeXRaw m)]) rest') | n' == m =
			Just (TeXEmpty, rest')
		findEnd n' (TeXSeq x y) = first (TeXSeq x) . findEnd n' y
		findEnd _ _ = Nothing
recognizeEnvs (TeXSeq x y) = TeXSeq (recognizeEnvs x) (recognizeEnvs y)
recognizeEnvs z = z

eval :: Macros -> LaTeX -> (LaTeX, Macros)
eval macros@Macros{..} l = case l of

	TeXEnv e a stuff
		| Just Environment{..} <- lookup (Text.pack e) environments
		, not (Text.pack e `elem` dontEval) -> (, mempty) $
			(if e == "TableBase" then TeXEnv e [] else id) $
			fst $ eval macros $
			recognizeEnvs $
				replArgs ((fst . eval macros . texFromArg . a) ++ defaultArgs) begin
					-- Note: Just appending defaultArgs is wrong in general but
					--       correct for the couple of uses we're dealing with.
				++ stuff
				++ end
		| otherwise -> (, mempty) $ TeXEnv e (mapTeXArg (fst . eval macros) . a) (fst $ eval macros stuff)

	TeXRaw _ -> (l, mempty)

	TeXCommS "ungap" -> mempty

	TeXComm "newcounter" [FixArg name']
		| name <- extractText name' ->
			(mempty, Macros mempty mempty (Map.singleton name 0))

	TeXComm "setcounter" [FixArg name', FixArg newValue']
		| name <- extractText name'
		, newValue <- extractText newValue' ->
			(mempty, Macros mempty mempty (Map.singleton name (read $ Text.unpack newValue)))

	TeXComm "addtocounter" [FixArg name', FixArg  addend']
		| name <- extractText name'
		, addend <- extractText addend'
		, Just value <- lookup name counters ->
			(mempty, Macros mempty mempty (Map.singleton name (value + (read $ Text.unpack addend))))

	TeXComm "value" [FixArg name']
		| name <- extractText name'
		, Just value <- lookup name counters ->
			(TeXRaw $ Text.pack $ show value, macros)
		| otherwise -> error $ "value: No such counter: " ++ show (concatRaws name')

	TeXComm "newenvironment" [FixArg (TeXRaw name), FixArg b, FixArg e]
		-> (mempty, Macros mempty (Map.singleton name (Environment b e [])) mempty)
	TeXComm "newenvironment" [FixArg (TeXRaw name), OptArg _, FixArg b, FixArg e]
		-> (mempty, Macros mempty (Map.singleton name (Environment b e [])) mempty)
	TeXComm "newenvironment" [FixArg (TeXRaw name), OptArg _, OptArg x, FixArg b, FixArg e]
		-> (mempty, Macros mempty (Map.singleton name (Environment b e [x])) mempty)
	TeXComm "newenvironment" x -> error $ "unrecognized newenv: " ++ show x
	TeXComm "newcommand" (FixArg (TeXCommS s) : _)
		| Text.pack s `elem` dontEval -> mempty
	TeXComm "newcommand" [FixArg (TeXCommS name), OptArg (TeXRaw argcount), FixArg body]
		-> (mempty, Macros (Map.singleton name (Command (read (Text.unpack argcount)) body)) mempty mempty)
	TeXComm "newcommand" [FixArg (TeXCommS name), FixArg body]
		-> (mempty, Macros (Map.singleton name (Command 0 body)) mempty mempty)
	TeXComm c args
		| Just Command{..} <- lookup c commands
		, length args >= arity ->
			let
				(x,y) = splitAt arity args
				body' :: LaTeX
				body' = replArgs (map (fst . eval macros . texFromArg) x) body
			in
				(, mempty) $ fst (eval macros body') ++
					mconcat (map (fst . eval macros . texFromArg) y)

	TeXComm c [] -> eval macros (TeXComm c [FixArg TeXEmpty])
	TeXComm c args -> (TeXComm c (map (mapTeXArg (fst . eval macros)) args), mempty)
	TeXCommS c
		| Just Command{..} <- lookup c commands
		, arity == 0 -> eval macros body
	TeXSeq x y ->
		let (x', m) = eval macros x in
		let (y', m') = eval (m ++ macros) y in
			(x' ++ y', m' ++ m)
	TeXBraces x -> (TeXBraces $ fst $ eval macros x, mempty)
	TeXMath t m -> (TeXMath t $ fst $ eval macros m, mempty)
	_ -> (l, mempty)

mapTeX :: (LaTeX -> Maybe LaTeX) -> (LaTeX -> LaTeX)
mapTeX f = texmap (isJust . f) (fromJust . f)

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
	. fst . eval macros
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
	assignNumbers s (RawEnumerated x p) = Enumerated x . assignNumbers s p
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
				return Paragraph{paraInItemdescr = rawParaInItemdescr, ..}
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
				return Paragraph{paraInItemdescr = rawParaInItemdescr, ..}
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


class Tables a where tables :: a -> [Table]

instance Tables a => Tables [a] where tables = concatMap tables
instance Tables a => Tables (Maybe a) where tables = maybe [] tables

instance Tables Section where
	tables Section{..} = (paragraphs >>= paraElems >>= tables) ++ (subsections >>= tables)

instance Tables Element where
	tables (TableElement t) = [t]
	tables _ = []

class Figures a where figures :: a -> [Figure]

instance Figures a => Figures [a] where figures = concatMap figures
instance Figures a => Figures (Maybe a) where figures = maybe [] figures

instance Figures Section where
	figures Section{..} = (paragraphs >>= paraElems >>= figures) ++ (subsections >>= figures)

instance Figures Element where
	figures (FigureElement f) = [f]
	figures _ = []

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
		resolve g (Enumerated s ps) = Enumerated s $ map (map (resolve g)) ps
		resolve g (Bnf n b) = Bnf n $ bnfGrammarterms g b
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

stripInfix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
stripInfix p s | Just r <- stripPrefix p s = Just ([], r)
stripInfix p (h:t) = first (h:) . stripInfix p t
stripInfix _ _  = Nothing

texStripInfix :: Text -> LaTeX -> Maybe (LaTeX, LaTeX)
texStripInfix t = go
	where
		go (TeXRaw (Text.unpack -> stripInfix (Text.unpack t) -> Just ((Text.pack -> x), (Text.pack -> y))))
			= Just (TeXRaw x, TeXRaw y)
		go (TeXSeq x y)
			| Just (x', x'') <- go x = Just (x', TeXSeq x'' y)
			| Just (y', y'') <- go y = Just (TeXSeq x y', y'')
		go _ = Nothing


parseIndex :: LaTeX -> (IndexPath, Maybe IndexKind)
parseIndex = go . mapTeXRaw unescapeIndexPath . concatRaws

	where
		go (texStripInfix "|seealso" -> Just (x, y)) = (parseIndexPath x, Just $ SeeAlso y)
		go (texStripInfix "|see" -> Just (x, y)) = (parseIndexPath x, Just $ See y)
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

data IndexComponent = IndexComponent { indexKey, indexFormatting :: LaTeX }
	deriving (Eq, Show)

instance Ord IndexComponent where
	compare = compare `on` (\c -> (f (indexKey c), f (indexFormatting c)))
		where
			g :: Char -> Int
			g c
				| isAlphaNum c = ord (toLower c) + 1000
				| otherwise = ord c
			f = map g . Text.unpack . indexKeyContent

indexKeyContent :: LaTeX -> Text
indexKeyContent = ikc
	where
		ikc (TeXRaw t) = replace "\n" " " t
		ikc (TeXSeq x y) = ikc x ++ ikc y
		ikc TeXEmpty = ""
		ikc (TeXComm "texttt" [FixArg x]) = ikc x
		ikc (TeXComm "textit" [FixArg x]) = ikc x
		ikc (TeXComm "mathsf" [FixArg x]) = ikc x
		ikc (TeXCommS "xspace") = "_"
		ikc (TeXCommS "Cpp") = "C++"
		ikc (TeXCommS "&") = "&"
		ikc (TeXCommS "%") = "%"
		ikc (TeXCommS "-") = ""
		ikc (TeXCommS "ell") = "ℓ"
		ikc (TeXCommS "~") = "~"
		ikc (TeXCommS "#") = "#"
		ikc (TeXCommS "{") = "{"
		ikc (TeXCommS "}") = "}"
		ikc (TeXCommS s)
			| Just c <- List.lookup s greekAlphabet = Text.pack [c]
		ikc (TeXCommS "tilde") = "~"
		ikc (TeXCommS "^") = "^"
		ikc (TeXCommS "\"") = "\""
		ikc (TeXCommS "") = ""
		ikc (TeXCommS "x") = "TODO"
		ikc (TeXCommS "textbackslash") = "\\";
		ikc (TeXCommS "textunderscore") = "_";
		ikc (TeXComm "discretionary" _) = ""
		ikc (TeXBraces x) = ikc x
		ikc (TeXMath Dollar x) = ikc x
		ikc (TeXComm "grammarterm_" [_, FixArg x]) = ikc x
		ikc x = error $ "indexKeyContent: unexpected: " ++ show x

type IndexPath = [IndexComponent]

data IndexKind = See LaTeX | SeeAlso LaTeX | IndexOpen | IndexClose
	deriving Show

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

withSubsections :: Section -> [Section]
withSubsections s = s : concatMap withSubsections (subsections s)

elemTex :: Element -> [LaTeX]
elemTex (LatexElements l) = l
elemTex (Enumerated _ e) = e >>= (>>= elemTex)
elemTex (Bnf _ l) = [l]
elemTex (Codeblock c) = [c]
elemTex (Minipage l) = l >>= elemTex
elemTex (Footnote _ c) = c >>= elemTex
elemTex (Tabbing t) = [t]
elemTex _ = [] -- todo: rest

sectionIndexEntries :: Section -> [RawIndexEntry]
sectionIndexEntries s =
	[ RawIndexEntry{..}
	| indexSection <- withSubsections s
	, [OptArg (TeXRaw indexCategory), FixArg (parseIndex -> (rawIndexPath, rawIndexKind))]
		<- paragraphs indexSection >>= paraElems >>= elemTex >>= lookForCommand "index" ] ++
	[ RawIndexEntry{indexCategory="generalindex", ..}
	| indexSection <- withSubsections s
	, [FixArg _, FixArg (parseIndex -> (rawIndexPath, rawIndexKind))]
		<- paragraphs indexSection >>= paraElems >>= elemTex >>= lookForCommand "defnx" ]

type IndexCategory = Text

type Index = Map IndexCategory IndexTree

indexCatName :: (Eq b , IsString a, IsString b) => b -> a
indexCatName "impldefindex" = "Index of implementation-defined behavior"
indexCatName "libraryindex" = "Index of library names"
indexCatName "generalindex" = "Index"
indexCatName _ = error "indexCatName"

data IndexEntry = IndexEntry
	{ indexEntrySection :: Section
	, indexEntryKind :: Maybe IndexKind
	, indexPath :: IndexPath }

type IndexTree = Map IndexComponent IndexNode

data IndexNode = IndexNode
	{ indexEntries :: [IndexEntry]
	, indexSubnodes :: IndexTree }

data Draft = Draft
	{ commitUrl :: Text
	, chapters  :: [Section]
	, index     :: Index }

tableByAbbr :: Draft -> LaTeX -> Maybe Table
	-- only returns Maybe because some of our tables are broken
tableByAbbr d a = listToMaybe [ t | t <- tables d, a `elem` tableAbbrs t ]

figureByAbbr :: Draft -> LaTeX -> Figure
figureByAbbr d a = case [ f | f <- figures d, a == figureAbbr f ] of
	[f] -> f
	_ -> error $ "figureByAbbr: " ++ show a

instance Figures Draft where figures = figures . chapters
instance Tables Draft where tables = tables . chapters
instance Sections Draft where sections = concatMap sections . chapters

splitChapters :: Draft -> ([Section], [Section])
splitChapters = span ((/= "library") . abbreviation) . chapters

coreChapters, libChapters :: Draft -> [Section]
coreChapters = fst . splitChapters
libChapters = snd . splitChapters

toIndex :: RawIndexEntry -> Index
toIndex RawIndexEntry{..} = Map.singleton indexCategory $ go rawIndexPath
	where
		go :: [IndexComponent] -> IndexTree
		go [c] = Map.singleton c (IndexNode [IndexEntry indexSection rawIndexKind rawIndexPath] Map.empty)
		go (c:cs) = Map.singleton c $ IndexNode [] $ go cs
		go _ = error "toIndex"

mergeIndices :: [Index] -> Index
mergeIndices = Map.unionsWith (Map.unionWith mergeIndexNodes)

mergeIndexNodes :: IndexNode -> IndexNode -> IndexNode
mergeIndexNodes x y = IndexNode
	{ indexEntries = indexEntries x ++ indexEntries y
	, indexSubnodes = Map.unionWith mergeIndexNodes (indexSubnodes x) (indexSubnodes y) }

load14882 :: IO Draft
load14882 = do

	commitUrl <- getCommitUrl

	macros@Macros{..} <-
		(initialMacros ++)
		. snd . eval mempty
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
			"grammar limits compatibility future charname"

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
