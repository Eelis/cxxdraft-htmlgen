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

data Element = LatexElements [LaTeX] | Itemized [Paragraph] | Enumerated [Paragraph]

-- We don't represent examples as elements with nested content
-- because sometimes they span multiple (numbered) paragraphs.

type Paragraph = [Element]

data SectionKind = NormalSection { _level :: Int } | InformativeAnnexSection | NormativeAnnexSection
	deriving Eq

data ChapterKind = NormalChapter | InformativeAnnex | NormativeAnnex
	deriving Eq

data LinearSection = LinearSection
	{ lsectionAbbreviation :: LaTeX
	, lsectionKind :: SectionKind
	, lsectionName :: LaTeX
	, lsectionPreamble :: [LaTeX]
		-- TODO: shouldn't this (and below) be a Paragraph, so it can have itemizeds?
	, lsectionParagraphs :: [Paragraph] }

type Chapter = (ChapterKind, Section)

data Section = Section
	{ abbreviation :: LaTeX
	, sectionName :: LaTeX
	, preamble :: [LaTeX]
	, paragraphs :: [Paragraph]
	, subsections :: [Section] }

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

isItemize :: LaTeX -> Bool
isItemize (TeXEnv "itemize" _ _) = True
isItemize _ = False

isEnumerate :: LaTeX -> Bool
isEnumerate (TeXEnv "enumerate" _ _) = True
isEnumerate (TeXEnv "enumeraten" _ _) = True
isEnumerate (TeXEnv "enumeratea" _ _) = True
isEnumerate _ = False

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

parseItems :: [LaTeX] -> [Paragraph]
parseItems [] = []
parseItems (TeXCommS "item" : more) = parsePara a : parseItems b
	where
		(a, b) = span (not . itemEnd) more
		itemEnd (TeXCommS "item") = True
		itemEnd _ = False
parseItems _ = error "need items or nothing"

parsePara :: [LaTeX] -> Paragraph
parsePara [] = []
parsePara (e@(TeXEnv _ [] items) : more)
	| isEnumerate e = Enumerated (parseItems $ dropWhile isJunk $ rmseqs items) : parsePara more
parsePara (TeXEnv "itemize" [] items : more) =
	Itemized (parseItems $ dropWhile isJunk $ rmseqs items) : parsePara more
parsePara x = LatexElements v : parsePara more
	where (v, more) = span (\y -> not (isItemize y || isEnumerate y)) x

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
		(lsectionPreamble, more') = span (not . isParaEnd) more
		(lsectionParagraphs, more'') = parseParas more'
parseSections (TeXComm "infannex" [
                               FixArg lsectionAbbreviation,
                               FixArg lsectionName]
              : more)
		= first (LinearSection{..} :) (parseSections more'')
	where
		lsectionKind = InformativeAnnexSection
		(lsectionPreamble, more') = span (not . isParaEnd) more
		(lsectionParagraphs, more'') = parseParas more'
parseSections (TeXComm "rSec" [OptArg (TeXRaw level),
                               OptArg lsectionAbbreviation,
                               FixArg lsectionName]
              : more)
		= first (LinearSection{..} :) (parseSections more'')
	where
		lsectionKind = NormalSection $ read $ Text.unpack level
		(lsectionPreamble, more') = span (not . isParaEnd) more
		(lsectionParagraphs, more'') = parseParas more'
parseSections x = ([], x)

files :: [FilePath]
files = [ "../../source/" ++ chap ++ ".tex"
        | chap <-  words $
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
