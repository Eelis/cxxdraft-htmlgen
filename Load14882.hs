{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, LambdaCase, TupleSections #-}

module Load14882 (CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Paragraph, ChapterKind(..), Section(..), Chapter, Draft(..), load14882) where

import Text.LaTeX.Base.Parser
import qualified Text.LaTeX.Base.Render as TeXRender
import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), lookForCommand, matchEnv, matchCommand, (<>), texmap)
import Data.Text (Text, replace)
import qualified Data.Text as Text
import Data.Monoid (Monoid(..), mconcat)
import Control.Monad (forM)
import qualified Prelude
import qualified Data.Text.IO
import Prelude hiding (take, (.), takeWhile, (++), lookup)
import Data.Char (isSpace, ord, isDigit)
import Control.Arrow (first)
import Data.Map (Map, keys, lookup)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Data.List (sort)
import Data.Maybe (isJust, fromJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Text.Regex (mkRegex, subRegex)

(++) :: Monoid a => a -> a -> a
(++) = mappend

data CellSpan = Normal | Multicolumn { width :: Int, colspec :: LaTeX } deriving (Eq, Show)
data Cell = Cell { cellSpan :: CellSpan, content :: Paragraph } deriving (Eq, Show)
data RowSepKind = RowSep | CapSep | NoSep deriving (Eq, Show)
data Row = Row { rowSep :: RowSepKind, cells :: [Cell] } deriving (Eq, Show)

data Element
	= LatexElements [LaTeX]
	| Enumerated String [Paragraph]
	| Bnf String LaTeX
	| Table 
		{ tableCaption :: LaTeX
		, columnSpec :: LaTeX
		, tableAbbrs :: [LaTeX]
		, tableBody :: [Row] }
	| Figure { figureName, figureAbbr :: LaTeX, figureSvg :: Text }
	deriving (Eq, Show)

-- We don't represent examples as elements with nested content
-- because sometimes they span multiple (numbered) paragraphs.

type Paragraph = [Element]

data SectionKind
	= NormalSection { _level :: Int }
	| DefinitionSection
	| InformativeAnnexSection
	| NormativeAnnexSection
	deriving (Eq, Show)

data ChapterKind = NormalChapter | InformativeAnnex | NormativeAnnex
	deriving (Eq, Show)

data LinearSection = LinearSection
	{ lsectionAbbreviation :: LaTeX
	, lsectionKind :: SectionKind
	, lsectionName :: LaTeX
	, lsectionPreamble :: Paragraph
	, lsectionParagraphs :: [Paragraph] }
	deriving Show

type Chapter = (ChapterKind, Section)

data Section = Section
	{ abbreviation :: LaTeX
	, sectionName :: LaTeX
	, preamble :: Paragraph
	, paragraphs :: [Paragraph]
	, subsections :: [Section] }
	deriving Show

lsectionLevel :: LinearSection -> Int
lsectionLevel (lsectionKind -> NormalSection l) = l
lsectionLevel (lsectionKind -> DefinitionSection) = 2
lsectionLevel _ = 0

treeizeChapters :: [LinearSection] -> [Chapter]
treeizeChapters [] = []
treeizeChapters (LinearSection{..} : more) =
		(chapterKind, Section{..}) : treeizeChapters more'
	where
		chapterKind
			| lsectionKind == InformativeAnnexSection = InformativeAnnex
			| lsectionKind == NormativeAnnexSection = NormativeAnnex
			| otherwise = NormalChapter
		abbreviation = lsectionAbbreviation
		paragraphs = lsectionParagraphs
		sectionName = lsectionName
		preamble = lsectionPreamble
		(treeizeSections -> subsections, more') = span ((> 0) . lsectionLevel) more

treeizeSections :: [LinearSection] -> [Section]
treeizeSections [] = []
treeizeSections (s@LinearSection{..} : more) =
		Section{..} : treeizeSections more'
	where
		abbreviation = lsectionAbbreviation
		paragraphs = lsectionParagraphs
		sectionName = lsectionName
		preamble = lsectionPreamble
		n = lsectionLevel s
		(treeizeSections -> subsections, more') = span ((> n) . lsectionLevel) more

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

rmseqs :: LaTeX -> [LaTeX]
rmseqs (TeXSeq x y) = rmseqs x ++ rmseqs y
rmseqs (TeXEnv "itemdescr" [] x) = rmseqs x
rmseqs (TeXEnv "paras" [] x) = rmseqs x
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

isFigure :: LaTeX -> Bool
isFigure (TeXEnv "importgraphic" _ _) = True
isFigure _ = False

isComment :: LaTeX -> Bool
isComment (TeXComment _) = True
isComment _ = False

isTeXComm :: String -> LaTeX -> Bool
isTeXComm x (TeXComm y _) | x == y = True
isTeXComm _ _ = False

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

parseItems :: [LaTeX] -> [Paragraph]
parseItems [] = []
parseItems (x : more)
	| isItem x = parsePara a : parseItems b
	where
		(a, b) = span (not . isItem) more
parseItems _ = error "need items or nothing"

isElementsEnd :: LaTeX -> Bool
isElementsEnd l = isEnumerate l /= Nothing || isBnf l || isTable l || isFigure l

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

parseTable :: LaTeX -> [Row]
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

makeRow :: LaTeX -> Row
makeRow l = Row sep $ makeRowCells l
	where
		sep
			| rowHas (== "hline") l = RowSep
			| rowHas (== "capsep") l = CapSep
			| otherwise = NoSep

makeRowCells :: LaTeX -> [Cell]
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

parsePara :: [LaTeX] -> Paragraph
parsePara [] = []
parsePara (e@(TeXEnv k a stuff) : more)
	| isFigure e
	, [FixArg figureName, FixArg figureAbbr, FixArg (TeXRaw figureFile)] <- a
	= Figure{figureSvg=loadFigure figureFile, ..} : parsePara more
	| isTable e
	, ((x : _todo) : _) <- lookForCommand "caption" stuff
	, (_, (FixArg y : _), content) : _todo <- matchEnv isTableEnv stuff
	= Table
		(texFromArg x)
		y
		(map (texFromArg . head) (lookForCommand "label" stuff))
		(parseTable content)
	  : parsePara more
	| isTable e = error $ "other table: " ++ show e

	| isBnf e = Bnf k stuff : parsePara more
	| Just ek <- isEnumerate e = Enumerated ek (parseItems $ dropWhile isJunk $ rmseqs stuff) : parsePara more
parsePara x = LatexElements v : parsePara more
	where (v, more) = span (not . isElementsEnd) (dropWhile isJunk x)

parseParas :: [LaTeX] -> ([Paragraph], [LaTeX])
parseParas (TeXCommS "pnum" : more)
		= first (parsePara para :) (parseParas more')
	where (para, more') = span (not . isParaEnd) more
parseParas x = ([], x)

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

killVerb :: String -> String
killVerb ('\\':'v':'e':'r':'b':'|':x) = "<verb>" ++ killVerb (tail $ dropWhile (/= '|') x)
killVerb (x:y) = x : killVerb y
killVerb [] = []

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
		go pre (TeXBraces b) = (ppp pre $ TeXBraces (concatRaws b), "")
		go pre (TeXComment t) = (ppp pre $ TeXComment t, "")
		go pre (TeXMath m t) = (ppp pre $ TeXMath m t, "")
		go pre t@(TeXLineBreak m b) = (ppp pre t, "")
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
dontEval = map Text.pack $ bnfEnvs ++ words "drawing definition Cpp importgraphic bottomline capsep"

filterMacros :: (String -> Bool) -> Macros -> Macros
filterMacros p Macros{..} = Macros
	(Map.filterWithKey (\k _ -> p k) commands)
	(Map.filterWithKey (\k _ -> p $ Text.unpack k) environments)
	(Map.filterWithKey (\k _ -> p $ Text.unpack k) counters)

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
	_ -> (l, mempty)

mapTeX :: (LaTeX -> Maybe LaTeX) -> (LaTeX -> LaTeX)
mapTeX f = texmap (isJust . f) (fromJust . f)

fixCommentsInCodeblocks :: LaTeX -> LaTeX
fixCommentsInCodeblocks = mapTeX $
	\case
		TeXEnv "codeblock" [] body -> Just $ TeXEnv "codeblock" [] $ mapTeX f body
		_ -> Nothing
	where
		f :: LaTeX -> Maybe LaTeX
		f (TeXComment t) = Just $ TeXRaw $ "%" ++ t ++ "\n"
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
	fixCommentsInCodeblocks
	. moreArgs
	. either (error "latex parse error") id
	. parseLaTeX

newlineCurlies :: Text -> Text
newlineCurlies =
	replace "\n{" "{"
	. replace "\n\t{" "{"
	. replace "\n {" "{"
	. replace "\n  {" "{"
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
	. Text.pack . killVerb . Text.unpack

getCommitUrl :: IO Text
getCommitUrl = do
	url <- Text.strip . Text.pack . readProcess "git" ["config", "--get", "remote.origin.url"] ""
	commit <- Text.strip . Text.pack . readProcess "git" ["rev-parse", "HEAD"] ""
	return $ Text.replace ".git" "/commit/" url ++ commit

data Draft = Draft { commitUrl :: Text, chapters :: [Chapter] }

load14882 :: IO Draft
load14882 = do

	commitUrl <- getCommitUrl

	m@Macros{..} <-
		snd
		. eval mempty
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
			"overloading templates exceptions preprocessor lib-intro "  ++
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

	let chapters = treeizeChapters $ mconcat sections

	return Draft{..}
