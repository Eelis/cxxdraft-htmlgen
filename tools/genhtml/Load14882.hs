{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Load14882 (Element(..), Paragraph, ChapterKind(..), Section(..), Chapter, load14882) where

import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..))
import Data.Text (replace)
import qualified Data.Text as Text
import Data.Monoid (mconcat)
import qualified Prelude
import qualified Data.Text.IO
import Prelude hiding (take, length, (.), head, takeWhile)
import Data.Char (isSpace)
import Control.Arrow (first)

data Element
	= LatexElements [LaTeX]
	| Enumerated String [Paragraph]
	| Bnf String LaTeX
	| Table String [TeXArg] LaTeX
	deriving Show

-- We don't represent examples as elements with nested content
-- because sometimes they span multiple (numbered) paragraphs.

type Paragraph = [Element]

data SectionKind = NormalSection { _level :: Int } | InformativeAnnexSection | NormativeAnnexSection
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
isTable (TeXEnv s _ _) | s `elem` tables = True
isTable _ = False

isComment :: LaTeX -> Bool
isComment (TeXComment _) = True
isComment _ = False

isTeXComm :: String -> LaTeX -> Bool
isTeXComm x (TeXComm y _) | x == y = True
isTeXComm _ _ = False

isParaEnd :: LaTeX -> Bool
isParaEnd (TeXCommS "pnum") = True
isParaEnd (TeXComm "rSec" _) = True
isParaEnd (TeXComm "infannex" _) = True
isParaEnd (TeXComm "normannex" _) = True
isParaEnd _ = False

isJunk :: LaTeX -> Bool
isJunk (TeXRaw x) = all isSpace (Text.unpack x)
isJunk (TeXComm "indextext" _) = True
isJunk (TeXComm "indexlibrary" _) = True
isJunk (TeXComment _) = True
isJunk _ = False

isItem :: LaTeX -> Bool
isItem (TeXCommS "item") = True
isItem (TeXComm "item" _) = True
isItem (TeXComm "stage" _) = True
isItem _ = False
	-- Todo: render the different kinds of items properly

tables :: [String]
tables = words "floattable tokentable libsumtab libsumtabbase libefftab longlibefftab libefftabmean longlibefftabmean libefftabvalue longlibefftabvalue liberrtab longliberrtab libreqtab1 libreqtab2 libreqtab2a libreqtab3 libreqtab3a libreqtab3b libreqtab3c libreqtab3d libreqtab3e libreqtab3f libreqtab4 libreqtab4a libreqtab4b libreqtab4c libreqtab4d libreqtab5 LibEffTab longLibEffTab libtab2 libsyntab2 libsyntab3 libsyntab4 libsyntab5 libsyntab6 libsyntabadd2 libsyntabadd3 libsyntabadd4 libsyntabadd5 libsyntabadd6 libsyntabf2 libsyntabf3 libsyntabf4 libsyntabf5 concepttable simpletypetable LongTable"

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
	| isTable e = Table k u stuff : parsePara more
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
parseSections x = ([], x)

files :: [FilePath]
files = [ "../../source/" ++ chap ++ ".tex"
        | chap <- words $
            "intro lex basic conversions expressions statements " ++
            "declarations declarators classes derived access special " ++
            "overloading templates exceptions preprocessor lib-intro " ++
            "support diagnostics utilities strings locales containers " ++
            "iterators algorithms numerics iostreams regex atomics threads " ++
            "grammar limits compatibility future charname xref" ]

killVerb :: String -> String
killVerb ('\\':'v':'e':'r':'b':'|':x) = "<verb>" ++ killVerb (tail $ dropWhile (/= '|') x)
killVerb (x:y) = x : killVerb y
killVerb [] = []

load14882 :: IO [Chapter]
load14882 = do

	s <- mconcat . mapM Data.Text.IO.readFile files

	let
		(sections, []) =
			parseSections
			$ filter (not . isTeXComm "index")
			$ filter (not . isTeXComm "indextext")
			$ filter (not . isTeXComm "indexlibrary")
			$ filter (not . isTeXComm "enlargethispage")
			$ filter (not . isTeXComm "indextext")
			$ filter (not . isTeXComm "indexdefn")
			$ filter (not . isComment)
			$ rmseqs
			$ either (error "latex parse error") id
			$ parseLaTeX
			$ replace "\\hspace*" "\\hspace"
			$ replace "\n{" "{"
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

	return (treeizeChapters sections)
