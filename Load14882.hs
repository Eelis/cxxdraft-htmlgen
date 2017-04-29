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

import qualified LaTeXParser as Parser
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import LaTeXBase
	( LaTeXUnit(..), LaTeX, TeXArg, ArgKind(..), lookForCommand
	, mapTeX, mapTeXRaw, concatRaws, texStripInfix, allUnits)
import Data.Text (Text, replace, isPrefixOf)
import Data.Text.IO (readFile)
import qualified Data.Text as Text
import Control.Monad (forM)
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (isAlpha)
import Control.Arrow (first)
import Data.Map (Map, keys)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Data.List (sort, unfoldr)
import System.Process (readProcess)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState, evalState, get, put, liftM2)
import Util ((.), (++), mapLast, stripInfix)
import RawDocument
import Document


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

-- In the LaTeX sources, \definition is often preceded by corresponding \indexdefns.
-- Since we treat definitions like sections (and generate pages for them), we need
-- to move the \indexdefns inside (after) the \definition, so that the index entries
-- don't link to the page for the preceding section.

moveIndexEntriesIntoDefs :: Text -> Text
moveIndexEntriesIntoDefs = Text.unlines . go . Text.lines
	where
		go :: [Text] -> [Text]
		go [] = []
		go (x:xs)
			| "\\indexdefn{" `isPrefixOf` x = case go xs of
				[] -> [x]
				y:ys
					| "\\definition{" `isPrefixOf` y -> y : x : ys
					| otherwise -> x : y : ys
			| otherwise = x : go xs

data Numbers = Numbers
	{ tableNr, figureNr, footnoteRefNr, footnoteNr
	, nextIndexEntryNr, noteNr, exampleNr :: Int }

class AssignNumbers a b | a -> b where
	assignNumbers :: forall m . (Functor m, MonadFix m, MonadState Numbers m) => Section -> a -> m b

instance AssignNumbers TeXArg TeXArg where
	assignNumbers s (y, x) = (y, ) . assignNumbers s x

instance AssignNumbers LaTeXUnit LaTeXUnit where
	assignNumbers s (TeXEnv x y z) = liftM2 (TeXEnv x) (assignNumbers s y) (assignNumbers s z)
	assignNumbers _ (TeXComm "index" args) = do
		n <- get
		put n{nextIndexEntryNr = nextIndexEntryNr n + 1}
		return $ TeXComm "index" $ (FixArg, [TeXRaw $ Text.pack $ show $ nextIndexEntryNr n]) : args
	assignNumbers _ (TeXComm "defnx" args) = do
		n <- get
		put n{nextIndexEntryNr = nextIndexEntryNr n + 1}
		return $ TeXComm "defnx" $ (FixArg, [TeXRaw $ Text.pack $ show $ nextIndexEntryNr n]) : args
	assignNumbers _ (TeXComm "footnoteref" []) = do
		Numbers{..} <- get
		put Numbers{footnoteRefNr = footnoteRefNr+1, ..}
		return $ TeXComm "footnoteref" [(FixArg, [TeXRaw $ Text.pack $ show footnoteRefNr])]
	assignNumbers s (TeXComm x args) = TeXComm x . assignNumbers s args
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
	assignNumbers s (RawEnumerated x p) = Enumerated x . (Item Nothing .) . assignNumbers s p
	assignNumbers s (RawLatexElement x) = LatexElement . assignNumbers s x
	assignNumbers s (RawBnf x y) = Bnf x . assignNumbers s y
	assignNumbers _ (RawTabbing x) = return $ Tabbing x
	assignNumbers s (RawCodeblock x) = Codeblock . assignNumbers s x
	assignNumbers s (RawNote x) = do
		Numbers{..} <- get
		put Numbers{noteNr = noteNr+1, ..}
		x' <- assignNumbers s x
		return $ NoteElement $ Note noteNr x'
	assignNumbers s (RawExample x) = do
		Numbers{..} <- get
		put Numbers{exampleNr = exampleNr+1, ..}
		x' <- assignNumbers s x
		return $ ExampleElement $ Example exampleNr x'

instance AssignNumbers RawFootnote Footnote where
	assignNumbers s (RawFootnote t) = do
		Numbers{..} <- get
		put Numbers{footnoteNr = footnoteNr+1, ..}
		t' <- assignNumbers s t
		return $ Footnote{footnoteNumber=footnoteNr,footnoteContent=t'}

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
	Bool -> Int -> [LinearSection] -> m [Section]
treeizeChapters _ _ [] = return []
treeizeChapters annexes secNumber (LinearSection{..} : more) = mdo
		sectionFootnotes <- assignNumbers newSec lsectionFootnotes
		let newSec = Section{sectionKind=lsectionKind, secIndexEntries=rawIndexEntriesForSec newSec, ..}
		let pn = paraNumbers $ paraNumbered . lsectionParagraphs
		paragraphs <- forM (zip pn lsectionParagraphs) $ assignNumbers newSec
		subsections <- treeizeSections 1 chapter [newSec] lsubsections
		(newSec :) . treeizeChapters annexes' (sectionNumber + 1) more'
	where
		sectionNumber = if annexes' /= annexes then 0 else secNumber
		annexes' = chapter /= NormalChapter
		parents = []
		chapter
			| lsectionKind == InformativeAnnexSection = InformativeAnnex
			| lsectionKind == NormativeAnnexSection = NormativeAnnex
			| otherwise = NormalChapter
		abbreviation = lsectionAbbreviation
		sectionName = lsectionName
		(lsubsections, more') = span ((> 0) . lsectionLevel) more

rawIndexEntriesForSec :: Section -> IntMap IndexEntry
rawIndexEntriesForSec s = IntMap.fromList
	[(n, e) | e@IndexEntry{indexEntryNr=Just n} <- sectionIndexEntries s]

assignItemNumbers :: Paragraph -> Paragraph
assignItemNumbers p
	| Just n <- paraNumber p = p{ paraElems = map (\x -> fst $ goElems [n,1] x) (paraElems p) }
	| otherwise = p
	where
		goElems :: [Int] -> [Element] -> ([Element], [Int])
		goElems nn [] = ([], nn)
		goElems nn (e:ee) = case e of
			Enumerated{..} ->
				let
					h l
						| enumCmd == "enumeratea" = map show (init l) ++ [[['a'..] !! (last l - 1)]]
						| otherwise = map show l
					items' = map (\(i, Item{..}) ->
						Item
							(Just (h $ mapLast (+i) nn))
							(map (\x -> fst $ goElems (mapLast (+i) nn ++ [1]) x) itemContent)
						) (zip [0..] enumItems)
				in
					first (Enumerated enumCmd items' :) (goElems (mapLast (+ length enumItems) nn) ee)
			_ -> first (e:) (goElems nn ee)

instance AssignNumbers (Maybe Int, RawParagraph) Paragraph where
	assignNumbers paraSection (paraNumber, RawParagraph{..}) = do
		nums <- get
		put nums{noteNr=1, exampleNr=1}
		paraElems <- assignNumbers paraSection rawParaElems
		return $ assignItemNumbers Paragraph
		  { paraInItemdescr = rawParaInItemdescr
		  , paraSourceLoc = rawParaSourceLoc
		  , .. }

treeizeSections :: forall m . (Functor m, MonadFix m, MonadState Numbers m) =>
	Int -> Chapter -> [Section] -> [LinearSection] -> m [Section]
treeizeSections _ _ _ [] = return []
treeizeSections sectionNumber chapter parents
	(s@LinearSection{..} : (span ((> lsectionLevel s) . lsectionLevel) -> (lsubsections, more'))) = mdo
		let newSec = Section
			{ sectionKind = lsectionKind
			, secIndexEntries = rawIndexEntriesForSec newSec
			, sectionName = lsectionName
			, abbreviation = lsectionAbbreviation
			, .. }
		let pn = paraNumbers $ paraNumbered . lsectionParagraphs
		sectionFootnotes <- assignNumbers newSec lsectionFootnotes
		paragraphs <- forM (zip pn lsectionParagraphs) $ assignNumbers newSec
		subsections <- treeizeSections 1 chapter (newSec : parents) lsubsections
		(newSec :) . treeizeSections (sectionNumber + 1) chapter parents more'

instance AssignNumbers a b => AssignNumbers [a] [b] where
	assignNumbers s = mapM (assignNumbers s)

type GrammarLinks = Map Text Section

nontermdefsInSection :: Section -> GrammarLinks
nontermdefsInSection s@Section{..} =
	Map.unions $
	((Map.fromList $ map (, s) (paragraphs >>= paraElems >>= (>>= nontermdefsInElement)))
	: map nontermdefsInSection subsections)

nontermdefsInElement :: Element -> [Text]
nontermdefsInElement (LatexElement e) = nontermdefs [e]
nontermdefsInElement (Bnf _ e) = nontermdefs e
nontermdefsInElement _ = []

nontermdefs :: LaTeX -> [Text]
nontermdefs t = [name | TeXComm "nontermdef" [(FixArg, [TeXRaw name])] <- allUnits t]

resolveGrammarterms :: GrammarLinks -> Section -> Section
resolveGrammarterms links Section{..} =
	Section{
		paragraphs  = map (\p -> p{paraElems = map (map resolve) (paraElems p)}) paragraphs,
		subsections = map (resolveGrammarterms links) subsections,
		sectionFootnotes = map resolveFN sectionFootnotes,
		..}
	where
		resolveFN :: Footnote -> Footnote
		resolveFN fn@Footnote{..} = fn{footnoteContent = map (map resolve) footnoteContent}
		resolve :: Element -> Element
-- TODO		resolve (LatexElement e) = LatexElement $ grammarterms links e
		resolve (Enumerated s ps) = Enumerated s $ map f ps
			where f i@Item{..} = i{itemContent=map (map resolve) itemContent}
		resolve (Bnf n b) = Bnf n $ grammarterms links $ bnfGrammarterms links b
		resolve other = other

grammarterms :: GrammarLinks -> LaTeX -> LaTeX
grammarterms links = mapTeX (go links)
	where
		go g (TeXComm "grammarterm" args@((FixArg, [TeXRaw name]) : _))
			| Just Section{..} <- Map.lookup (Text.toLower name) g =
				Just [TeXComm "grammarterm_" ((FixArg, abbreviation) : args)]
		go _ _ = Nothing

bnfGrammarterms :: GrammarLinks -> LaTeX -> LaTeX
bnfGrammarterms links = (>>= go) . mapTeX wordify
	where
		wordify :: LaTeXUnit -> Maybe LaTeX
		wordify (TeXRaw stuff) = Just $ map TeXRaw $ unfoldr f stuff
			where
				f s | Text.null s = Nothing
				f s | isName $ Text.head s = Just $ Text.span isName s
				f s = Just $ Text.break isName s

				isName c = isAlpha c || c `elem` ['-', '_']
		wordify _ = Nothing

		go :: LaTeXUnit -> LaTeX
		go n@(TeXRaw name)
			| Just Section{..} <- Map.lookup name links =
				[TeXComm "grammarterm_" [(FixArg, abbreviation), (FixArg, [n])]]
		go x = [x]

parseIndex :: LaTeX -> (IndexPath, Maybe IndexKind)
parseIndex = go . mapTeXRaw unescapeIndexPath . concatRaws
	where
		go (texStripInfix "|seealso" -> Just (x, [TeXBraces y])) = (parseIndexPath x, Just $ See True y)
		go (texStripInfix "|see " -> Just (x, [TeXBraces y])) = (parseIndexPath x, Just $ See False y)
		go (texStripInfix "|see" -> Just (x, [TeXBraces y])) = (parseIndexPath x, Just $ See False y)
		go (texStripInfix "|(" -> Just (t, _)) = (parseIndexPath t, Just IndexOpen)
		go (texStripInfix "|)" -> Just (t, _)) = (parseIndexPath t, Just IndexClose)
		go t = (parseIndexPath t, Nothing)

		unescapeIndexPath :: Text -> LaTeXUnit
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
		parseIndexPath t = [IndexComponent [] t]

sectionTex :: Section -> LaTeX
sectionTex s =
	((paragraphs s >>= paraElems) ++ (sectionFootnotes s >>= footnoteContent)) >>= (>>= elemTex)

sectionIndexEntries :: Section -> [IndexEntry]
sectionIndexEntries s =
	[ IndexEntry{..}
	| indexEntrySection <- sections s
	, [(FixArg, [TeXRaw (Text.unpack -> read -> Just -> indexEntryNr)]), (OptArg, [TeXRaw indexCategory]), (FixArg, (parseIndex -> (indexPath, indexEntryKind)))]
		<- lookForCommand "index" (sectionTex indexEntrySection)] ++
	[ IndexEntry
		{ indexCategory = "generalindex"
		, indexEntryKind = Just DefinitionIndex
		, ..}
	| indexEntrySection <- sections s
	, [(FixArg, [TeXRaw (Text.unpack -> read -> Just -> indexEntryNr)]), (FixArg, _), (FixArg, (parseIndex -> (indexPath, Nothing)))]
		<- lookForCommand "defnx" (sectionTex indexEntrySection)]

toIndex :: IndexEntry -> Index
toIndex IndexEntry{..} = Map.singleton indexCategory $ go indexPath
	where
		go :: [IndexComponent] -> IndexTree
		go [c] = Map.singleton c (IndexNode [IndexEntry indexEntrySection indexEntryKind indexPath indexEntryNr indexCategory] Map.empty)
		go (c:cs) = Map.singleton c $ IndexNode [] $ go cs
		go _ = error "toIndex"

trackPnums :: FilePath -> Text -> Text
	-- Replaces \pnum with \pnum{file}{line}
trackPnums file = Text.pack . unlines . map (uncurry f) . zip [1..] . lines . Text.unpack
	where
		f :: Integer -> String -> String
		f lineNr line
			| Just (pre, post) <- stripInfix "\\pnum" line
				= pre ++ "\\pnum{" ++ file ++ "}{" ++ show lineNr ++ "}" ++ post
			| otherwise = line

load14882 :: IO Draft
load14882 = do

	commitUrl <- getCommitUrl

	macros@Parser.Macros{..} <- loadMacros

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
			moveIndexEntriesIntoDefs .
			trackPnums p .
			readFile p

		extra <-
			if c /= "grammar" then return ""
			else replace "\\gramSec" "\\rSec1" . readFile "std-gram.ext"

		let r = parseFile macros (stuff ++ extra)

		putStrLn $ show (length r) ++ " sections"
		return r

	if length (show secs) == 0 then undefined else do
		-- force eval before we leave the dir
		let
			chapters = evalState (treeizeChapters False 1 $ mconcat secs) (Numbers 1 1 1 1 0 1 1)
			ntdefs = Map.unions $ map nontermdefsInSection chapters
			chapters' = map (resolveGrammarterms ntdefs) chapters
			allEntries :: [IndexEntry]
			allEntries = chapters' >>= sectionIndexEntries
			index = mergeIndices $ map toIndex allEntries
			indexEntryMap = IntMap.fromList [(n, e) | e@IndexEntry{indexEntryNr=Just n} <- allEntries]

		return Draft{chapters=chapters', ..}
