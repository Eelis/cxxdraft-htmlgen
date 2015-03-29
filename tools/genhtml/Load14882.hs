{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Load14882 (Element(..), Paragraph, ChapterKind(..), Section(..), Chapter, load14882) where

import Text.LaTeX.Base.Parser
import qualified Text.LaTeX.Base.Render as TeXRender
import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), lookForCommand)
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
import Data.List (sort)

(++) :: Monoid a => a -> a -> a
(++) = mappend

data Element
	= LatexElements [LaTeX]
	| Enumerated String [Paragraph]
	| Bnf String LaTeX
	| Table { tableCaption :: LaTeX, tableAbbrs :: [LaTeX], tableBody :: LaTeX }
	deriving Show

-- We don't represent examples as elements with nested content
-- because sometimes they span multiple (numbered) paragraphs.

type Paragraph = [Element]

data SectionKind = NormalSection { _level :: Int } | DefinitionSection | InformativeAnnexSection | NormativeAnnexSection
	deriving Eq

data ChapterKind = NormalChapter | InformativeAnnex | NormativeAnnex
	deriving (Eq, Show)

data LinearSection = LinearSection
	{ lsectionAbbreviation :: LaTeX
	, lsectionKind :: SectionKind
	, lsectionName :: LaTeX
	, lsectionPreamble :: Paragraph
	, lsectionParagraphs :: [Paragraph] }

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

isBnf :: LaTeX -> Bool
isBnf (TeXEnv s _ _)
	| s `elem` ["bnf", "bnfkeywordtab", "bnftab"] = True
isBnf _ = False

isTable :: LaTeX -> Bool
isTable (TeXEnv "TableBase" _ _) = True
isTable _ = False

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
isItem (TeXComm "stage" _) = True
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
isElementsEnd l = isEnumerate l /= Nothing || isBnf l || isTable l

parsePara :: [LaTeX] -> Paragraph
parsePara [] = []
parsePara (e@(TeXEnv k u stuff) : more)
	| isTable e
	, [x : todo] <- lookForCommand "caption" stuff
	= Table
		(texFromArg x)
		(map (texFromArg . head) (lookForCommand "label" stuff))
		stuff
	  : parsePara more
	| isTable e = error $ "other table: " ++ show e
	| isBnf e = Bnf k stuff : parsePara more
	| Just ek <- isEnumerate e = Enumerated ek (parseItems $ dropWhile isJunk $ rmseqs stuff) : parsePara more
parsePara x = LatexElements v : parsePara more
	where (v, more) = span (not . isElementsEnd) x

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
mapTeXArg f x = x

texFromArg :: TeXArg -> LaTeX
texFromArg (FixArg t) = t
texFromArg (OptArg t) = t
texFromArg (SymArg t) = t
texFromArg _ = error "no"

data Macros = Macros
	{ commands :: Map String Command
	, environments :: Map Text Environment }
	deriving Show

instance Monoid Macros where
	mempty = Macros mempty mempty
	mappend x y = Macros (commands x ++ commands y) (environments x ++ environments y)

getDigit :: Char -> Maybe Int
getDigit c
	| isDigit c = Just $ ord c - ord '0'
	| otherwise = Nothing

replaceArgs :: [LaTeX] -> String -> LaTeX
replaceArgs args (span (/= '#') -> (before, '#':c:more))
	| Just i <- getDigit c =
		TeXRaw (Text.pack before) ++
		((args ++ repeat (TeXRaw "wtf")) !! (i-1)) ++
		replaceArgs args more
replaceArgs args s = TeXRaw $ Text.pack s

dontEval :: [Text]
dontEval = map Text.pack $ words "TableBase bnf bnftab imporgraphic drawing definition Cpp"

eval :: Macros -> [LaTeX] -> LaTeX -> (LaTeX, Macros)
eval macros@Macros{..} arguments l = case l of

	TeXEnv e a stuff -> eval macros arguments $
		TeXComm "begin" (FixArg (TeXRaw (Text.pack e)) : a) ++ stuff
		++ TeXComm "end" [FixArg (TeXRaw (Text.pack e))]

	TeXRaw s -> (replaceArgs arguments (Text.unpack s), mempty)

	TeXComm "newenvironment" (FixArg (TeXRaw s) : _)
		| s `elem` dontEval -> mempty
	TeXComm "newenvironment" [FixArg (TeXRaw name), FixArg b, FixArg e]
		-> (mempty, Macros mempty (Map.singleton name (Environment b e)))
	TeXComm "newenvironment" [FixArg (TeXRaw name), OptArg _, FixArg b, FixArg e]
		-> (mempty, Macros mempty (Map.singleton name (Environment b e)))
	TeXComm "newenvironment" _ -> error "unrecognized newenv"

	TeXComm "newcommand" (FixArg (TeXCommS s) : _)
		| Text.pack s `elem` dontEval -> mempty
	TeXComm "newcommand" [FixArg (TeXCommS name), OptArg (TeXRaw argcount), FixArg body]
		-> (mempty, Macros (Map.singleton name (Command (read (Text.unpack argcount)) body)) mempty)
	TeXComm "newcommand" [FixArg (TeXCommS name), FixArg body]
		-> (mempty, Macros (Map.singleton name (Command 0 body)) mempty)
	TeXComm c args
		| Just Command{..} <- lookup c commands
		, length args >= arity ->
			let (x,y) = splitAt arity args in
			(fst (eval macros (map (fst . eval macros arguments . texFromArg) x) body) ++
				mconcat (map (fst . eval macros arguments . texFromArg) y), mempty)

	TeXComm "begin" (FixArg (TeXRaw n) : a)
		| Just Environment{..} <- lookup n environments ->
			(fst (eval macros (map (fst . eval macros arguments . texFromArg) a) begin), mempty)
	TeXComm "end" [FixArg (TeXRaw n)]
		| Just Environment{..} <- lookup n environments ->
			(fst (eval macros arguments end), mempty)

	TeXComm c [] -> eval macros arguments (TeXComm c [FixArg TeXEmpty])
	TeXComm c args -> (TeXComm c (map (mapTeXArg (fst . eval macros arguments)) args), mempty)
	TeXCommS c
		| Just Command{..} <- lookup c commands
		, arity == 0 -> eval macros [] body
	TeXSeq x y ->
		let (x', m) = eval macros arguments x in
		let (y', m') = eval (m ++ macros) arguments y in
			(x' ++ y', m' ++ m)
	_ -> (l, mempty)

moreArgs :: LaTeX -> LaTeX
moreArgs (TeXSeq (TeXComm n a) (TeXSeq (TeXBraces x) more))
	= moreArgs (TeXSeq (TeXComm n (a ++ [FixArg x])) more)
moreArgs (TeXComm n a) = TeXComm n (map (mapTeXArg moreArgs) a)
moreArgs (TeXSeq x y) = moreArgs x ++ moreArgs y
moreArgs (TeXEnv e a x) = TeXEnv e (map (mapTeXArg moreArgs) a) (moreArgs x)
moreArgs (TeXBraces x) = TeXBraces (moreArgs x)
moreArgs x = x

doParseLaTeX :: Text -> LaTeX
doParseLaTeX =
	moreArgs
	. either (error "latex parse error") id
	. parseLaTeX

parseFile :: Macros -> Text -> [LinearSection]
parseFile macros s = sections
	where
		(sections, _) =
			parseSections
			$ filter (not . isTeXComm "index")
			$ filter (not . isTeXComm "indextext")
			$ filter (not . isTeXComm "indexlibrary")
			$ filter (not . isTeXComm "enlargethispage")
			$ filter (not . isTeXComm "indextext")
			$ filter (not . isTeXComm "indexdefn")
			$ filter (not . isComment)
			$ rmseqs
			$ doParseLaTeX
			$ TeXRender.render afterExec
		afterExec =
			doParseLaTeX
			$ TeXRender.render
			$ fst . eval macros []
			$ doParseLaTeX
			$ replace "\\hspace*" "\\hspace"
			$ replace "\n{" "{"
			$ replace "\n\t{" "{"
			$ replace "``" "“"
			$ replace "''" "”"
			$ replace "\\rSec0" "\\rSec[0]"
			$ replace "\\rSec1" "\\rSec[1]"
			$ replace "\\rSec2" "\\rSec[2]"
			$ replace "\\rSec3" "\\rSec[3]"
			$ replace "\\rSec4" "\\rSec[4]"
			$ replace "\\rSec5" "\\rSec[5]"
			$ replace "\\bigl[" "\\bigl ["
			$ Text.pack $ killVerb $ Text.unpack
			$ s

load14882 :: IO [Chapter]
load14882 = do

	m@Macros{..} <-
		snd
		. eval mempty []
		. doParseLaTeX
		. replace "\n{" "{"
		. replace "\n {" "{"
		. replace "\n\t{" "{"
		. mconcat
		. mapM Data.Text.IO.readFile
		[ "../../source/config.tex"
		, "../../source/macros.tex"
		, "../../source/tables.tex" ]

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
			"grammar limits compatibility future charname xref"

	putStrLn "Loading chapters"
	sections <- forM files $ \c -> do
		let p = "../../source/" ++ c ++ ".tex"
		putStr $ "  " ++ c ++ "... "; hFlush stdout

		r <- parseFile m . Data.Text.IO.readFile p
		putStrLn $ show (length r) ++ " sections"
		return r

	return $ treeizeChapters $ mconcat sections
