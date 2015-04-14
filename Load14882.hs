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
	OverlappingInstances,
	RecursiveDo #-}

module Load14882 (
	CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Paragraph,
	Section(..), Chapter(..), Draft(..), Table(..), Figure(..),
	LaTeX,
	load14882) where

import Text.LaTeX.Base.Parser
import qualified Text.LaTeX.Base.Render as TeXRender
import Text.LaTeX.Base (protectString)
import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), lookForCommand, matchEnv, matchCommand, (<>), texmap)
import Data.Text (Text, replace)
import qualified Data.Text as Text
import Data.Monoid (Monoid(..), mconcat)
import Control.Monad (forM)
import qualified Prelude
import qualified Data.Text.IO
import Prelude hiding (take, (.), takeWhile, (++), lookup)
import Data.Char (isSpace, ord, isDigit, isAlpha)
import Control.Arrow (first)
import Data.Map (Map, keys, lookup)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Data.List (sort, unfoldr)
import Data.Maybe (isJust, fromJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Text.Regex (mkRegex, subRegex)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState, evalState, get, put, liftM2)

(++) :: Monoid a => a -> a -> a
(++) = mappend

data CellSpan = Normal | Multicolumn { width :: Int, colspec :: LaTeX } deriving Show
data Cell a = Cell { cellSpan :: CellSpan, content :: a } deriving Show
data RowSepKind = RowSep | CapSep | Clines [(Int, Int)] | NoSep deriving Show
data Row a = Row { rowSep :: RowSepKind, cells :: [Cell a] } deriving Show

data RawElement
	= RawLatexElements [LaTeX]
	| RawItemdescr [RawElement]
	| RawEnumerated String [RawParagraph]
	| RawBnf String LaTeX
	| RawTable
		{ rawTableCaption :: LaTeX
		, rawColumnSpec :: LaTeX
		, rawTableAbbrs :: [LaTeX]
		, rawTableBody :: [Row RawParagraph] }
	| RawTabbing LaTeX
	| RawFigure { rawFigureName :: LaTeX, rawFigureAbbr :: LaTeX, rawFigureSvg :: Text }
	| RawFootnote RawParagraph
	| RawCodeblock LaTeX
	deriving Show

data Table = Table
	{ tableNumber :: Int
	, tableCaption :: LaTeX
	, columnSpec :: LaTeX
	, tableAbbrs :: [LaTeX]
	, tableBody :: [Row Paragraph]
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
	| Itemdescr [Element]
	| Enumerated String [Paragraph]
	| Bnf String LaTeX
	| TableElement Table
	| Tabbing LaTeX
	| FigureElement Figure
	| Footnote { footnoteNumber :: Int, footnoteContent :: Paragraph }
	| Codeblock { code :: LaTeX }
	deriving Show

-- We don't represent examples as elements with nested content
-- because sometimes they span multiple (numbered) paragraphs.

type RawParagraph = [RawElement]
type Paragraph = [Element]

data SectionKind
	= NormalSection { _level :: Int }
	| DefinitionSection
	| InformativeAnnexSection
	| NormativeAnnexSection
	deriving (Eq, Show)

data Chapter = NormalChapter | InformativeAnnex | NormativeAnnex
	deriving (Eq, Show)

data LinearSection = LinearSection
	{ lsectionAbbreviation :: LaTeX
	, lsectionKind :: SectionKind
	, lsectionName :: LaTeX
	, lsectionPreamble :: RawParagraph
	, lsectionParagraphs :: [RawParagraph] }
	deriving Show

data Section = Section
	{ abbreviation :: LaTeX
	, sectionName :: LaTeX
	, preamble :: Paragraph
	, paragraphs :: [Paragraph]
	, subsections :: [Section]
	, sectionNumber :: Int
	, chapter :: Chapter
	, parents :: [Section] -- if empty, this is the chapter
	}
	deriving Show

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

rmseqs :: LaTeX -> [LaTeX]
rmseqs (TeXSeq x y) = rmseqs x ++ rmseqs y
rmseqs (TeXEnv "itemdescr" [] x) = makeItemdescrs $ dropWhile isJunk $ rmseqs x
	where
		makeItemdescrs [] = []
		makeItemdescrs ((TeXCommS "pnum") : rest) =
			(TeXCommS "pnum") : (TeXEnv "itemdescr_" [] (foldl1 TeXSeq para)) : (makeItemdescrs rest')
			where
				(para, rest') = span (not . isParaEnd) rest
		makeItemdescrs other = other
rmseqs x = [x]

isEnumerate :: LaTeX -> Maybe String
isEnumerate (TeXEnv s _ _)
	| s `elem` ["enumeraten", "enumeratea", "enumerate", "itemize", "description"] = Just s
isEnumerate _ = Nothing

bnfEnvs :: [String]
bnfEnvs = ["bnf", "bnfkeywordtab", "bnftab", "simplebnf"]

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

isComment :: LaTeX -> Bool
isComment (TeXComment _) = True
isComment _ = False

isParaEnd :: LaTeX -> Bool
isParaEnd (TeXCommS "pnum") = True
isParaEnd (TeXComm "definition" _) = True
isParaEnd (TeXComm "rSec" _) = True
isParaEnd (TeXComm "infannex" _) = True
isParaEnd (TeXComm "normannex" _) = True
isParaEnd _ = False

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

parseItems :: [LaTeX] -> [RawParagraph]
parseItems [] = []
parseItems (x : more)
	| isItem x = parsePara a : parseItems b
	where
		(a, b) = span (not . isItem) more
parseItems _ = error "need items or nothing"

isElementsEnd :: LaTeX -> Bool
isElementsEnd l =
	isEnumerate l /= Nothing || isBnf l || isTable l
	|| isTabbing l || isFigure l || isCodeblock l

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

parseTable :: LaTeX -> [Row RawParagraph]
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

makeRow :: LaTeX -> Row RawParagraph
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

makeRowCells :: LaTeX -> [Cell RawParagraph]
makeRowCells TeXEmpty = []
makeRowCells latex =
	case rest of
		TeXEmpty -> [makeCell cell]
		TeXSeq _ r ->
			(makeCell $ cell <> (TeXRaw cell')) : makeRowCells (TeXSeq (TeXRaw rest'') r)
		TeXRaw r ->
			[(makeCell $ cell <> (TeXRaw cell')), makeCell $ TeXRaw (rest'' ++ r)]
		_ -> error $ "Unexpected " ++ show rest
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

extractFootnotes :: [LaTeX] -> ([LaTeX], [RawElement])
extractFootnotes [] = ([], [])
extractFootnotes (e : es) = (e' : es', f ++ fs)
	where
		extract (TeXComm "footnote" [FixArg content]) =
			(TeXCommS "footnoteref", [RawFootnote $ parsePara $ rmseqs content])
		extract (TeXCommS "footnotemark") =
			(TeXCommS "footnoteref", [])
		extract (TeXComm "footnotetext" [FixArg content]) =
			(TeXEmpty, [RawFootnote $ parsePara $ rmseqs content])
		extract (TeXSeq a b) = extract a ++ extract b
		extract (TeXEnv env args content) = first (TeXEnv env args) (extract content)
		extract other = (other, [])
		(e', f) = extract e
		(es', fs) = extractFootnotes es

parsePara :: [LaTeX] -> RawParagraph
parsePara [] = []
parsePara ((TeXEnv "itemdescr_" [] stuff) : more) =
	(RawItemdescr $ parsePara $ rmseqs stuff') : footnotes ++ parsePara more
	where
		([stuff'], footnotes) = extractFootnotes [stuff]

parsePara (env@(TeXEnv _ _ _) : more) =
	go e' : parsePara more ++ footnotes
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
			| Just ek <- isEnumerate e = RawEnumerated ek (parseItems $ dropWhile isJunk $ rmseqs stuff)
		go other = error $ "Unexpected " ++ show other

		([e'], footnotes) = extractFootnotes [env]

parsePara x = RawLatexElements v' : parsePara more ++ footnotes
	where
		(v, more) = elems (dropWhile isJunk x)
		(v', footnotes) = extractFootnotes v

elems :: [LaTeX] -> ([LaTeX], [LaTeX])
elems [] = ([], [])
elems y@(x:xs)
	| isElementsEnd x = ([], y)
	| TeXRaw (Text.breakOn "\n\n" -> (a, b)) <- x
	, b /= "" = ([TeXRaw a], TeXRaw (Text.drop 2 b) : xs)
	| otherwise = first (x :) (elems xs)

parseParas :: [LaTeX] -> ([RawParagraph], [LaTeX])
parseParas stuff = (goFootnotes paras [], rest)
	where
		(paras, rest) = collectParas stuff

		goFootnotes :: [[LaTeX]] -> [RawElement] -> [RawParagraph]
		goFootnotes [] _ = []
		goFootnotes [p] fs = [(parsePara p') ++ (fs ++ f)]
			where (p', f) = extractFootnotes p
		goFootnotes (p : ps) fs = (parsePara p') : goFootnotes ps (fs ++ f)
			where (p', f) = extractFootnotes p

		collectParas :: [LaTeX] -> ([[LaTeX]], [LaTeX])
		collectParas (TeXCommS "pnum" : more) = first (p :) (collectParas more')
			where (p, more') = span (not . isParaEnd) more
		collectParas x = ([], x)

parseSections :: [LaTeX] -> ([LinearSection], [LaTeX])
parseSections (TeXComm "normannex" [
                               FixArg lsectionAbbreviation,
                               FixArg lsectionName]
              : more)
		= first (LinearSection{..} :) (parseSections more'')
	where
		lsectionKind = NormativeAnnexSection
		(parsePara -> lsectionPreamble, more') = span (not . isParaEnd) more
		(lsectionParagraphs, more'') = parseParas more'
parseSections (TeXComm "infannex" [
                               FixArg lsectionAbbreviation,
                               FixArg lsectionName]
              : more)
		= first (LinearSection{..} :) (parseSections more'')
	where
		lsectionKind = InformativeAnnexSection
		(parsePara -> lsectionPreamble, more') = span (not . isParaEnd) more
		(lsectionParagraphs, more'') = parseParas more'
parseSections (TeXComm "rSec" [OptArg (TeXRaw level),
                               OptArg lsectionAbbreviation,
                               FixArg lsectionName]
              : more)
		= first (LinearSection{..} :) (parseSections more'')
	where
		lsectionKind = NormalSection $ read $ Text.unpack level
		(parsePara -> lsectionPreamble, more') = span (not . isParaEnd) more
		(lsectionParagraphs, more'') = parseParas more'
parseSections (TeXComm "definition" [FixArg lsectionName,
                                     FixArg lsectionAbbreviation]
              : more)
		= first (LinearSection{..} :) (parseSections more'')
	where
		lsectionKind = DefinitionSection
		(parsePara -> lsectionPreamble, more') = span (not . isParaEnd) more
		(lsectionParagraphs, more'') = parseParas more'
parseSections x = ([], x)

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
	{ begin, end :: !LaTeX }
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
	{environments = Map.fromList
		[ ("ttfamily", Environment mempty mempty)
		, ("paras",    Environment mempty mempty)
		, ("minipage", Environment mempty mempty) ]}

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
				(TeXSeq x' y', s')
		(a, b) =  go "" l

replaceArgsInString :: [LaTeX] -> String -> LaTeX
replaceArgsInString args = concatRaws . go
	where
		go :: String -> LaTeX
		go ('#':'#':more) = TeXRaw "#" ++ go more
		go ('#':c:more)
			| Just i <- getDigit c =
			((args ++ repeat "wtf") !! (i-1)) ++
			go more
		go (c : more) = TeXRaw (Text.pack [c]) ++ go more
		go [] = TeXEmpty

dontEval :: [Text]
dontEval = map Text.pack $ bnfEnvs ++ words "drawing definition Cpp importgraphic bottomline capsep bigoh itemdescr grammarterm nontermdef"

eval :: Macros -> LaTeX -> (LaTeX, Macros)
eval macros@Macros{..} l = case l of

	TeXEnv e a stuff
		| Just Environment{..} <- lookup (Text.pack e) environments
		, not (Text.pack e `elem` dontEval) -> (, mempty) $
			(if e == "TableBase" then TeXEnv e [] else id) $
			fst $ eval macros $
			doParseLaTeX $ TeXRender.render $
				replArgs (fst . eval macros . texFromArg . a) begin
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
		-> (mempty, Macros mempty (Map.singleton name (Environment b e)) mempty)
	TeXComm "newenvironment" [FixArg (TeXRaw name), OptArg _, FixArg b, FixArg e]
		-> (mempty, Macros mempty (Map.singleton name (Environment b e)) mempty)
	TeXComm "newenvironment" _ -> error "unrecognized newenv"

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

flatten :: LaTeX -> LaTeX
flatten (TeXSeq (TeXSeq a b) c) = (flatten a) <> (flatten b) <> (flatten c)
flatten other = other

normalizeAtSigns :: LaTeX -> LaTeX
normalizeAtSigns = flatten . (mapTeX go)
	where
		go (TeXRaw stuff)
			| (before, after) <- Text.breakOn "@" stuff
			, after /= "" 
			, after' <- TeXRaw (Text.tail after) =
				Just $ (TeXRaw before) <> (TeXRaw "@") <> (maybe after' id $ go after')
			| otherwise = Nothing
		go _ = Nothing

mapOutsideAts :: (LaTeX -> Maybe LaTeX) -> LaTeX -> LaTeX
mapOutsideAts f = go . normalizeAtSigns
	where
		go (TeXSeq (TeXRaw "@") rest) = TeXSeq (TeXRaw "@") (dontGo rest)
		go (TeXSeq a b) = TeXSeq (go a) (go b)
		go other = maybe other id $ f other

		dontGo (TeXSeq (TeXRaw "@") rest) = TeXSeq (TeXRaw "@") (go rest)
		dontGo (TeXSeq a b) = TeXSeq a (dontGo b)
		dontGo other = other

reparseCodeblocks :: LaTeX -> LaTeX
reparseCodeblocks = mapTeX $
	\case
		TeXEnv "codeblock" [] body -> Just $ TeXEnv "codeblock" [] $ mapOutsideAts f body
		_ -> Nothing
	where
		f :: LaTeX -> Maybe LaTeX
		f (TeXComment t) = Just $ (TeXRaw "%") <> (mapOutsideAts f $ doParseLaTeX (t ++ "\n"))
		f (TeXCommS c) = Just $ TeXRaw $ ("\\" ++ Text.pack c)
		f (TeXLineBreak _ _) = Just $ TeXRaw "\\\\"
		f _ = Nothing

moreArgs :: LaTeX -> LaTeX
moreArgs (TeXSeq (TeXComm n a) (TeXSeq (TeXBraces x) more))
	= moreArgs (TeXSeq (TeXComm n (a ++ [FixArg x])) more)
moreArgs (TeXComm n a) = TeXComm n (map (mapTeXArg moreArgs) a)
moreArgs (TeXSeq x y) = moreArgs x ++ moreArgs y
moreArgs (TeXEnv e a x) 
	| otherwise = TeXEnv e (map (mapTeXArg moreArgs) a) (moreArgs x)
moreArgs (TeXBraces x) = TeXBraces (moreArgs x)
moreArgs x = x

doParseLaTeX :: Text -> LaTeX
doParseLaTeX =
	reparseCodeblocks
	. moreArgs
	. either (error "latex parse error") id
	. parseLaTeX

newlineCurlies :: Text -> Text
newlineCurlies =
	replace "\n{" "{"
	. replace "\n\t{" "{"
	. replace "\n {" "{"
	. replace "}\n  {" "}{"
		-- Todo: These are sometimes inappropriate...

parseFile :: Macros -> Text -> [LinearSection]
parseFile macros = fst
	. parseSections
	. filter (not . isComment)
	. rmseqs
	. doParseLaTeX
	. TeXRender.render
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
	url <- Text.strip . Text.pack . readProcess "git" ["config", "--get", "remote.origin.url"] ""
	commit <- Text.strip . Text.pack . readProcess "git" ["rev-parse", "HEAD"] ""
	return $
		( Text.replace "git@github.com:" "http://github.com/"
		$ Text.replace ".git" "/commit/" url)
		++ commit

-- Numbering

data Numbers = Numbers { tableNr, figureNr, footnoteRefNr, footnoteNr :: Int }

class AssignNumbers a b | a -> b where
	assignNumbers :: forall m . (Functor m, MonadFix m, MonadState Numbers m) => Section -> a -> m b

instance AssignNumbers LaTeX LaTeX where
	assignNumbers s (TeXSeq x y) = liftM2 TeXSeq (assignNumbers s x) (assignNumbers s y)
	assignNumbers s (TeXEnv x y z) = TeXEnv x y . assignNumbers s z
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
	assignNumbers s (RawItemdescr x) = Itemdescr . assignNumbers s x
	assignNumbers _ (RawBnf x y) = return $ Bnf x y
	assignNumbers _ (RawTabbing x) = return $ Tabbing x
	assignNumbers _ (RawCodeblock x) = return $ Codeblock x

lsectionLevel :: LinearSection -> Int
lsectionLevel (lsectionKind -> NormalSection l) = l
lsectionLevel (lsectionKind -> DefinitionSection) = 2
lsectionLevel _ = 0

treeizeChapters :: forall m . (Functor m, MonadFix m, MonadState Numbers m) =>
	Int -> [LinearSection] -> m [Section]
treeizeChapters _ [] = return []
treeizeChapters sectionNumber (LinearSection{..} : more) = mdo
		newSec <- return Section{..}
		preamble <- assignNumbers newSec lsectionPreamble
		paragraphs <- assignNumbers newSec lsectionParagraphs
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
		preamble <- assignNumbers newSec lsectionPreamble
		paragraphs <- assignNumbers newSec lsectionParagraphs
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


tablesInSection :: Section -> [Table]
tablesInSection Section{..} =
	concatMap tablesInElement (preamble ++ concat paragraphs)
	++ concatMap tablesInSection subsections
tablesInElement :: Element -> [Table]
tablesInElement (TableElement t) = [t]
tablesInElement _ = []

figuresInSection :: Section -> [Figure]
figuresInSection Section{..} =
	concatMap figuresInElement (preamble ++ concat paragraphs)
	++ concatMap figuresInSection subsections

figuresInElement :: Element -> [Figure]
figuresInElement (FigureElement f) = [f]
figuresInElement _ = []

type GrammarLinks = Map Text Section

nontermdefsInSection :: Section -> GrammarLinks
nontermdefsInSection s@Section{..} =
	Map.unions $
	((Map.fromList $ map (, s) (concatMap nontermdefsInElement (preamble ++ concat paragraphs)))
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
		paragraphs=map (map (resolve links)) paragraphs,
		preamble=map (resolve links) preamble,
		subsections=map (resolveGrammarterms links) subsections,
		..}
	where
		resolve :: GrammarLinks -> Element -> Element
		resolve g (LatexElements e) = LatexElements $ map (grammarterms g) e
		resolve g (Itemdescr e) = Itemdescr $ map (resolve g) e
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

data Draft = Draft
	{ commitUrl :: Text
	, chapters :: [Section]
	, tables :: [Table]
	, figures :: [Figure] }

load14882 :: IO Draft
load14882 = do

	commitUrl <- getCommitUrl

	m@Macros{..} <-
		(initialMacros ++)
		. snd . eval mempty
		. doParseLaTeX
		. newlineCurlies
		. mconcat
		. mapM Data.Text.IO.readFile
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
	sections <- forM files $ \c -> do
		let p = c ++ ".tex"
		putStr $ "  " ++ c ++ "... "; hFlush stdout

		r <- parseFile m . Data.Text.IO.readFile p

		putStrLn $ show (length r) ++ " sections"
		return r

	if length (show sections) == 0 then undefined else do -- force eval before we leave the dir

	let chapters = evalState (treeizeChapters 1 $ mconcat sections) (Numbers 1 1 1 1)
	let tables = concatMap tablesInSection chapters
	let figures = concatMap figuresInSection chapters

	let ntdefs = Map.unions $ map nontermdefsInSection chapters
	let chapters' = map (resolveGrammarterms ntdefs) chapters

	return Draft{chapters=chapters', ..}
