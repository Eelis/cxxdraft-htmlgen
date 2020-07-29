{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Document (
	CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Paragraph(..),
	Section(..), Chapter(..), Draft(..), Table(..), Figure(..), Item(..), Footnote(..),
	IndexPath, IndexComponent(..), IndexCategory, Index, IndexTree, IndexNode(..),
	IndexEntry(..), IndexKind(..), Note(..), Example(..), TeXPara(..), Sentence(..),
	texParaTex, texParaElems, XrefDelta, sectionByAbbr, isDefinitionSection, Abbreviation,
	indexKeyContent, indexCatName, Sections(sections), SectionKind(..), mergeIndices, SourceLocation(..),
	figures, tables, tableByAbbr, figureByAbbr, elemTex, footnotes, allElements, indexHeading,
	LaTeX) where

import LaTeXBase (LaTeXUnit(..), LaTeX, MathType(Dollar))
import Data.Text (Text, replace)
import qualified Data.Text as Text
import qualified Data.List as List
import Data.IntMap (IntMap)
import Data.Function (on)
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (ord, isAlpha, toLower, isDigit, isUpper, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.String (IsString)
import Util ((.), (++), greekAlphabet)

-- Document structure:

data CellSpan = Normal | Multicolumn { width :: Int, colspec :: LaTeX } deriving (Eq, Show)
data Cell a = Cell { cellSpan :: CellSpan, content :: a } deriving (Eq, Show)
data RowSepKind = RowSep | CapSep | Clines [(Int, Int)] | NoSep deriving (Eq, Show)
data Row a = Row { rowSep :: RowSepKind, cells :: [Cell a] } deriving (Eq, Show)

data Table = Table
	{ tableNumber :: Int
	, tableCaption :: LaTeX
	, columnSpec :: LaTeX
	, tableAbbrs :: [Abbreviation]
	, tableBody :: [Row [TeXPara]]
	, tableSection :: Section }

instance Show Table where
	show _ = "<table>"

data Figure = Figure
	{ figureNumber :: Int
	, figureName :: LaTeX
	, figureAbbr :: Abbreviation
	, figureSvg :: Text
	, figureSection :: Section }

instance Show Figure where
	show _ = "<figure>"

data Item = Item
	{ itemNumber :: Maybe [String]
	, itemLabel :: Maybe LaTeX
	, itemInlineContent :: [Element]
	, itemBlockContent :: [TeXPara] }
	deriving Show

itemElements :: Item -> [Element]
itemElements Item{..} = itemInlineContent ++ allElements itemBlockContent

data Footnote = Footnote
	{ footnoteNumber :: Int
	, footnoteContent :: [TeXPara] }
	deriving Show

data Note = Note { noteNumber :: Int, noteLabel :: Text, noteContent :: [TeXPara] }
	deriving Show

data Example = Example { exampleNumber :: Int, exampleContent :: [TeXPara] }
	deriving Show

data Sentence = Sentence { sentenceNumber :: Maybe Int, sentenceElems :: [Element] }
	deriving Show

newtype TeXPara = TeXPara { sentences :: [Sentence] }
	deriving Show

data Element
	= LatexElement LaTeXUnit
	| Enumerated { enumCmd :: String, enumItems :: [Item] }
	| Bnf String LaTeX
	| TableElement Table
	| Tabbing LaTeX
	| FigureElement Figure
	| Codeblock LaTeXUnit
	| NoteElement Note
	| ExampleElement Example
	deriving Show

data SectionKind
	= NormalSection { _level :: Int }
	| DefinitionSection { _level :: Int }
	| InformativeAnnexSection
	| NormativeAnnexSection
	deriving (Eq, Show)

isDefinitionSection :: SectionKind -> Bool
isDefinitionSection (DefinitionSection _) = True
isDefinitionSection _ = False

data Chapter = NormalChapter | InformativeAnnex | NormativeAnnex
	deriving (Eq, Show)

data SourceLocation = SourceLocation
	{ sourceFile :: FilePath
	, sourceLine :: Int }
	deriving (Eq, Show)

data Paragraph = Paragraph
	{ paraNumber :: Maybe Int
	, paraInItemdescr :: Bool
	, paraElems :: [TeXPara]
	, paraSection :: Section
	, paraSourceLoc :: Maybe SourceLocation
	, allParaElems :: [Element] } -- derivable but stored for efficiency
	deriving Show

type Abbreviation = Text -- of a section, figure, or table

data Section = Section
	{ abbreviation :: Abbreviation
	, sectionName :: LaTeX
	, paragraphs :: [Paragraph]
	, sectionFootnotes :: [Footnote]
	, subsections :: [Section]
	, sectionNumber :: Int
	, chapter :: Chapter
	, parents :: [Section] -- if empty, this is the chapter
	, sectionKind :: SectionKind
	, secIndexEntries :: IntMap IndexEntry
	}
	deriving Show

instance Eq Section where
	x == y = abbreviation x == abbreviation y

type XrefDelta = [(Abbreviation, [LaTeX])]

data Draft = Draft
	{ commitUrl :: Text
	, chapters  :: [Section]
	, index     :: Index
	, indexEntryMap :: IntMap IndexEntry
	, xrefDelta :: XrefDelta }

-- (The index entry maps are derivable but stored for efficiency.)

-- Indices:

data IndexComponent = IndexComponent { distinctIndexSortKey, indexKey :: LaTeX }
	deriving Show

instance Eq IndexComponent where
	x == y =
		distinctIndexSortKey x == distinctIndexSortKey y &&
		indexKeyContent (indexKey x) == indexKeyContent (indexKey y)

indexHeading :: IndexComponent -> String
indexHeading (indexSortKey -> indexKeyContent -> Text.head -> c)
	| isDigit c = "Numbers"
	| isAlpha c = [toUpper c]
	| otherwise = "Symbols"

type IndexPath = [IndexComponent]

data IndexKind = See { _also :: Bool, _ref :: LaTeX } | IndexOpen | IndexClose | BfPage
	deriving (Eq, Show)

type IndexCategory = Text

type Index = Map IndexCategory IndexTree

instance Show IndexEntry where
	show IndexEntry{..} =
		"IndexEntry"
		++ "{indexSection=" ++ show (sectionName indexEntrySection)
		++ ",indexCategory=" ++ show indexCategory
		++ ",indexPath=" ++ show indexPath
		++ "}"

indexSortKey :: IndexComponent -> LaTeX
indexSortKey IndexComponent{..}
	| distinctIndexSortKey /= [] = distinctIndexSortKey
	| otherwise = indexKey

data IndexEntry = IndexEntry
	{ indexEntrySection :: Section
	, indexEntryKind :: Maybe IndexKind
	, indexPath :: IndexPath
	, indexEntryNr :: Maybe Int
	, indexCategory :: Text
	, isDefinitionIndexEntry :: Bool
	}

type IndexTree = Map IndexComponent IndexNode

data IndexNode = IndexNode
	{ indexEntries :: [IndexEntry]
	, indexSubnodes :: IndexTree }

instance Ord IndexComponent where
	compare = compare `on` (\c -> (f (indexSortKey c), f (indexKey c)))
		where
			g :: Char -> (Int, Int)
			g c
				| isDigit c = (1, ord c)
				| isAlpha c = (2, ord (toLower c))
				| otherwise = (0, ord c)
			h :: String -> ([(Int, Int)], Int)
			h x = (map g x, if isUpper (head x) then 0 else 1)
			f = h . Text.unpack . indexKeyContent

mergeIndices :: [Index] -> Index
mergeIndices = Map.unionsWith (Map.unionWith mergeIndexNodes)

mergeIndexNodes :: IndexNode -> IndexNode -> IndexNode
mergeIndexNodes x y = IndexNode
	{ indexEntries = indexEntries x ++ indexEntries y
	, indexSubnodes = Map.unionWith mergeIndexNodes (indexSubnodes x) (indexSubnodes y) }

indexKeyContent :: LaTeX -> Text
indexKeyContent txt = mconcat (map ikc txt)
	where
		ikc :: LaTeXUnit -> Text
		ikc (TeXRaw t) = replace "\n" " " t
		ikc (TeXComm "tcode" _ [(_, x)]) = indexKeyContent x
		ikc (TeXComm "noncxxtcode" _ [(_, x)]) = indexKeyContent x
		ikc (TeXComm "texttt" _ [(_, x)]) = indexKeyContent x
		ikc (TeXComm "textit" _ [(_, x)]) = indexKeyContent x
		ikc (TeXComm "textsc" _ [(_, x)]) = indexKeyContent x
		ikc (TeXComm "mathsf" _ [(_, x)]) = indexKeyContent x
		ikc (TeXComm "textsf" _ [(_, x)]) = indexKeyContent x
		ikc (TeXComm "textcolor" _ [_, (_, x)]) = indexKeyContent x
		ikc (TeXComm "xspace" _ []) = "_"
		ikc (TeXComm "Cpp" _ []) = "C++"
		ikc (TeXComm "&" _ []) = "&"
		ikc (TeXComm "%" _ []) = "%"
		ikc (TeXComm "-" _ []) = ""
		ikc (TeXComm "ell" _ []) = "ℓ"
		ikc (TeXComm "~" _ []) = "~"
		ikc (TeXComm "#" _ []) = "#"
		ikc (TeXComm "{" _ []) = "{"
		ikc (TeXComm "}" _ []) = "}"
		ikc (TeXComm "caret" _ []) = "^"
		ikc (TeXComm "tilde" _ []) = "~"
		ikc (TeXComm "^" _ []) = "^"
		ikc (TeXComm "\"" _ []) = "\""
		ikc (TeXComm "" _ []) = ""
		ikc (TeXComm "x" _ []) = "TODO"
		ikc (TeXComm "textbackslash" _ []) = "\\"
		ikc (TeXComm "textunderscore" _ []) = "_"
		ikc (TeXComm "discretionary" _ _) = ""
		ikc (TeXComm "texorpdfstring" _ [_, (_, x)]) = indexKeyContent x
		ikc (TeXComm s _ [])
			| Just c <- List.lookup s greekAlphabet = Text.pack [c]
		ikc (TeXBraces x) = indexKeyContent x
		ikc (TeXMath Dollar x) = indexKeyContent x
		ikc (TeXComm "grammarterm_" _ [_, (_, x)]) = indexKeyContent x
		ikc (TeXComm "grammarterm" _ [(_, x)]) = indexKeyContent x
		ikc x = error $ "indexKeyContent: unexpected: " ++ show x

indexCatName :: (Eq b, Show b, IsString a, IsString b) => b -> a
indexCatName "impldefindex" = "Index of implementation-defined behavior"
indexCatName "libraryindex" = "Index of library names"
indexCatName "headerindex" = "Index of library headers"
indexCatName "generalindex" = "Index"
indexCatName "grammarindex" = "Index of grammar productions"
indexCatName "conceptindex" = "Index of library concepts"
indexCatName x = error $ "indexCatName: " ++ show x

-- Gathering entities:

class Sections a where sections :: a -> [Section]

instance Sections Section where sections s = s : (subsections s >>= sections)
instance Sections Draft where sections = concatMap sections . chapters
instance Sections a => Sections (Maybe a) where sections = maybe [] sections

allParagraphs :: Sections a => a -> [Paragraph]
allParagraphs = (>>= paragraphs) . sections

tables :: Sections a => a -> [(Paragraph, Table)]
tables x = [(p, t) | p <- allParagraphs x, TableElement t <- allParaElems p]

figures :: Sections a => a -> [(Paragraph, Figure)]
figures x = [(p, f) | p <- allParagraphs x, FigureElement f <- allParaElems p]

footnotes :: Sections a => a -> [(Section, Footnote)]
footnotes x = [(s, f) | s <- sections x, f <- sectionFootnotes s]

allElements :: [TeXPara] -> [Element]
allElements x = x >>= sentences >>= sentenceElems >>= f
	where
		f :: Element -> [Element]
		f e = e : case e of
			Enumerated {..} -> enumItems >>= itemElements
			TableElement Table{..} -> allElements $ tableBody >>= cells >>= content
			NoteElement Note{..} -> allElements noteContent
			Codeblock y -> [LatexElement y]
			ExampleElement Example{..} -> allElements exampleContent
			Tabbing y -> LatexElement . y
			Bnf _ y -> LatexElement . y
			_ -> []

-- Misc:

texParaElems :: TeXPara -> [Element]
texParaElems = (>>= sentenceElems) . sentences

texParaTex :: TeXPara -> LaTeX
texParaTex = (>>= elemTex) . texParaElems

itemTex :: Item -> LaTeX
itemTex Item{..} = (itemInlineContent >>= elemTex) ++ (itemBlockContent >>= texParaTex)

elemTex :: Element -> LaTeX
elemTex (NoteElement n) = noteContent n >>= texParaTex
elemTex (ExampleElement x) = exampleContent x >>= texParaTex
elemTex (LatexElement l) = [l]
elemTex (Enumerated _ e) = e >>= itemTex
elemTex (Bnf _ l) = l
elemTex (Tabbing t) = t
elemTex (Codeblock t) = [t]
elemTex (TableElement t) = tableBody t >>= rowTex
	where
		rowTex :: Row [TeXPara] -> LaTeX
		rowTex r = content . cells r >>= (>>= texParaTex)
elemTex (FigureElement _) = []

tableByAbbr :: Draft -> Abbreviation -> Maybe Table
	-- only returns Maybe because some of our tables are broken
tableByAbbr d a = listToMaybe [ t | (_, t) <- tables d, a `elem` tableAbbrs t ]

figureByAbbr :: Draft -> Abbreviation -> Figure
figureByAbbr d a = case [ f | (_, f) <- figures d, a == figureAbbr f ] of
	[f] -> f
	_ -> error $ "figureByAbbr: " ++ show a

sectionByAbbr :: Draft -> Abbreviation -> Maybe Section
sectionByAbbr d a = listToMaybe [ s | s <- sections d, a == abbreviation s ]
