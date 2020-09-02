{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE
	OverloadedStrings,
	ScopedTypeVariables,
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
import qualified Data.List as List
import Data.IntMap (IntMap)
import LaTeXBase
	( LaTeXUnit(..), LaTeX, TeXArg, ArgKind(..), lookForCommand
	, mapTeX, mapTeXRaw, concatRaws, texStripInfix, allUnits)
import Data.Text (Text, replace, isPrefixOf)
import Data.Text.IO (readFile)
import qualified Data.Text as Text
import Control.Monad (forM, when)
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (isAlpha)
import Control.Arrow (first)
import Data.Map (Map)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Data.List (unfoldr, (\\))
import System.Process (readProcess)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState, evalState, get, put, liftM2, modify)
import Util ((.), (++), mapLast, stripInfix, measure)
import RawDocument
import Sentences (splitIntoSentences, isActualSentence, breakSentence)
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

moveIndexEntriesIntoDefs :: [Text] -> [Text]
moveIndexEntriesIntoDefs [] = []
moveIndexEntriesIntoDefs (x:xs)
	| "\\indexdefn{" `isPrefixOf` x = case moveIndexEntriesIntoDefs xs of
		[] -> [x]
		y:ys
			| "\\definition{" `isPrefixOf` y -> y : x : ys
			| otherwise -> x : y : ys
	| otherwise = x : moveIndexEntriesIntoDefs xs

moveIndexEntriesIntoSecs :: [Text] -> [Text]
moveIndexEntriesIntoSecs = go []
	where
		go x [] = x
		go x (h:t)
		    | "\\indextext{" `isPrefixOf` h = go (h : x) t
		    | "\\rSec" `isPrefixOf` h = h : reverse x ++ go [] t
		    | otherwise = reverse x ++ [h] ++ go [] t

{- The document has a ton of:

  \indexlibraryglobal{bla}%
  \begin{itemdecl}
  void bla();
  \end{itemdecl}

To highlight the whole itemdecl, indexItemDecls converts this to:

  \begin{indexeditemdecl}{
  \indexlibraryglobal{bla}%
  }
  void bla();
  \end{indexeditemdecl}
-}

indexCodeEnvs :: [Text] -> [Text] -> [Text]
indexCodeEnvs envs = go []
	where
		go collected [] = collected
		go collected (x:xs)
			| "\\index" `isPrefixOf` x = go (collected ++ [x]) xs
			| [e] <- [e | e <- envs, ("\\begin{" ++ e ++ "}") `isPrefixOf` x] =
				let (code, _ : rest) = span (not . (("\\end{" ++ e ++ "}") `isPrefixOf`)) xs
				in (if null collected then
						["\\begin{" ++ e ++ "}"]
						++ code
						++ ["\\end{" ++ e ++ "}"]
					else
						["\\begin{indexed" ++ e ++ "}{"] ++ collected ++ ["}"]
						++ code
						++ ["\\end{indexed" ++ e ++ "}"])
				 	++ go [] rest
			| otherwise = collected ++ (x : go [] xs)

data Numbers = Numbers
	{ tableNr, figureNr, footnoteRefNr, footnoteNr, itemDeclNr
	, nextIndexEntryNr, noteNr, exampleNr, nextSentenceNr :: Int }

class AssignNumbers a b | a -> b where
	assignNumbers :: forall m . (Functor m, MonadFix m, MonadState Numbers m) => Section -> a -> m b

instance AssignNumbers TeXArg TeXArg where
	assignNumbers s (y, x) = (y, ) . assignNumbers s x

instance AssignNumbers LaTeXUnit LaTeXUnit where
	assignNumbers s (TeXEnv "itemdecl" [] x) = do
		n <- get
		put n{itemDeclNr = itemDeclNr n + 1}
		TeXEnv "itemdecl" [(FixArg, [TeXRaw $ Text.pack $ show $ itemDeclNr n])] . assignNumbers s x
	assignNumbers s (TeXEnv "indexeditemdecl" indices x) = do
		n <- get
		put n{itemDeclNr = itemDeclNr n + 1}
		liftM2 (TeXEnv "indexeditemdecl") (assignNumbers s indices) (assignNumbers s x)
	assignNumbers s (TeXEnv x y z) = liftM2 (TeXEnv x) (assignNumbers s y) (assignNumbers s z)
	assignNumbers _ (TeXComm "index" ws args) = do
		n <- get
		put n{nextIndexEntryNr = nextIndexEntryNr n + 1}
		return $ TeXComm "index" ws $ (FixArg, [TeXRaw $ Text.pack $ show $ nextIndexEntryNr n]) : args
	assignNumbers _ (TeXComm "defnx" ws args) = do
		n <- get
		put n{nextIndexEntryNr = nextIndexEntryNr n + 1}
		return $ TeXComm "defnx" ws $ (FixArg, [TeXRaw $ Text.pack $ show $ nextIndexEntryNr n]) : args
	assignNumbers _ (TeXComm "footnoteref" ws []) = do
		Numbers{..} <- get
		put Numbers{footnoteRefNr = footnoteRefNr+1, ..}
		return $ TeXComm "footnoteref" ws [(FixArg, [TeXRaw $ Text.pack $ show footnoteRefNr])]
	assignNumbers s (TeXComm x ws args) = TeXComm x ws . assignNumbers s args
	assignNumbers _ x = return x

instance AssignNumbers a b => AssignNumbers (Cell a) (Cell b) where
	assignNumbers s x@Cell{..} = do
		content' <- assignNumbers s content
		return x{content=content'}

instance AssignNumbers a b => AssignNumbers (Row a) (Row b) where
	assignNumbers s x@Row{..} = do
		cells' <- assignNumbers s cells
		return x{cells=cells'}

instance AssignNumbers RawTexPara TeXPara where
	assignNumbers s (RawTexPara (splitIntoSentences -> x)) = TeXPara . f x
		where
			f [] = return []
			f (h:t) = do
				h' <- assignNumbers s h
				let actual = isActualSentence h
				n <- get
				put n{nextSentenceNr = nextSentenceNr n + (if actual then 1 else 0)}
				(Sentence (if actual then Just (nextSentenceNr n) else Nothing) h' :) . f t

assignNonInlineItem :: (MonadState Numbers m, MonadFix m) => Section -> RawItem -> m Item
assignNonInlineItem s (RawItem label content) = do
	n <- get
	put n{nextSentenceNr = 1}
	Item Nothing (if null label then Nothing else Just label) [] . assignNumbers s content

breakFirstSentence :: [TeXPara] -> (Sentence, [TeXPara])
breakFirstSentence (TeXPara (x:y) : z) = (x, TeXPara y : z)
breakFirstSentence x = error $ "breakFirstSentence: " ++ show x

assignInlineItem :: (MonadState Numbers m, MonadFix m) => Section -> RawItem -> m Item
assignInlineItem s (RawItem label content) = do
	n <- get
	put n{nextSentenceNr = 1}
	content' <- assignNumbers s content
	let (Sentence _ x, y) = breakFirstSentence content'
	return $ Item Nothing (if null label then Nothing else Just label) x y

endsWithFullStop :: [RawElement] -> Bool
endsWithFullStop = isJust . breakSentence

instance AssignNumbers RawElement Element where
	assignNumbers section RawFigure{..} = do
		Numbers{..} <- get
		put Numbers{figureNr = figureNr+1, ..}
		return $ FigureElement Figure
			{ figureNumber  = figureNr
			, figureName    = rawFigureName
			, figureAbbr    = "fig:" ++ rawFigureAbbr
			, figureSvg     = rawFigureSvg
			, figureSection = section }
	assignNumbers s RawTable{..} = do
		Numbers{..} <- get
		put Numbers{tableNr = tableNr+1, ..}
		tableBody <- assignNumbers s rawTableBody
		return $ TableElement Table
			{ tableNumber  = tableNr
			, columnSpec   = rawColumnSpec
			, tableAbbr   = rawTableAbbr
			, tableCaption = rawTableCaption
			, tableSection = s
			, .. }
	assignNumbers s (RawEnumerated x p) = do
		origNum <- nextSentenceNr . get
		let c = length (filter (any (endsWithFullStop . rawTexParaElems) . rawItemContent) p)
		r <- mapM (if c > 1 then assignNonInlineItem s else assignInlineItem s) p
		modify $ \y -> y{nextSentenceNr = origNum}
		return $ Enumerated x r
	assignNumbers s (RawLatexElement x) = LatexElement . assignNumbers s x
	assignNumbers s (RawBnf x y) = Bnf x . assignNumbers s y
	assignNumbers _ (RawTabbing x) = return $ Tabbing x
	assignNumbers s (RawCodeblock x) = Codeblock . assignNumbers s x
	assignNumbers s (RawNote label x) = do
		Numbers{..} <- get
		put Numbers{noteNr = noteNr+1, ..}
		x' <- assignNumbers s x
		return $ NoteElement $ Note noteNr label x'
	assignNumbers s (RawExample x) = do
		Numbers{..} <- get
		put Numbers{exampleNr = exampleNr+1, ..}
		x' <- assignNumbers s x
		return $ ExampleElement $ Example exampleNr x'

instance AssignNumbers RawFootnote Footnote where
	assignNumbers s (RawFootnote t) = do
		Numbers{..} <- get
		put Numbers{footnoteNr = footnoteNr+1, nextSentenceNr = 1, ..}
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
		let
			ie = rawIndexEntriesForSec newSec
			newSec = Section{sectionKind=lsectionKind, secIndexEntries=ie, secIndexEntriesByPath=reverseIndexEntryMap ie, ..}
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

reverseIndexEntryMap :: IntMap IndexEntry -> Map IndexPath [(Int, IndexEntry)]
reverseIndexEntryMap m = Map.fromListWith (++) [(indexPath x, [(i, x)])  | (i, x) <- IntMap.assocs m]

assignItemNumbers :: Paragraph -> Paragraph
assignItemNumbers p
	| Just n <- paraNumber p = p{ paraElems = fst $ goParas [n, 1] $ paraElems p }
	| otherwise = p
	where

		goParas :: [Int] -> [TeXPara] -> ([TeXPara], [Int])
		goParas nn [] = ([], nn)
		goParas nn (TeXPara e : pp) = first (TeXPara e' :) (goParas nn' pp)
			where (e', nn') = goSentences nn e

		goSentences :: [Int] -> [Sentence] -> ([Sentence], [Int])
		goSentences nn [] = ([], nn)
		goSentences nn (Sentence m e : ss) = first (Sentence m e' :) (goSentences nn' ss)
			where (e', nn') = goElems nn e

		goElems :: [Int] -> [Element] -> ([Element], [Int])
		goElems nn [] = ([], nn)
		goElems nn (e:ee) = first (e' :) (goElems nn' ee)
			where (e', nn') = goElem nn e

		goElem :: [Int] -> Element -> (Element, [Int])
		goElem nn Enumerated{..} = (Enumerated enumCmd items', mapLast (+ length enumItems) nn)
			where
				h l
					| enumCmd == "enumeratea" = map show (init l) ++ [[['a'..] !! (last l - 1)]]
					| otherwise = map show l
				items' = map (\(i, Item{..}) ->
					Item
						(Just (h $ mapLast (+i) nn))
						itemLabel
						(fst $ goElems (mapLast (+i) nn ++ [1]) itemInlineContent)
						(fst $ goParas (mapLast (+i) nn ++ [1]) itemBlockContent)
					) (zip [0..] enumItems)
		goElem nn (NoteElement (Note nr label paras)) = (NoteElement (Note nr label paras'), nn')
			where (paras', nn') = goParas nn paras
		goElem nn (ExampleElement (Example nr paras)) = (ExampleElement (Example nr paras'), nn')
			where (paras', nn') = goParas nn paras
		goElem nn x = (x, nn)

instance AssignNumbers (Maybe Int, RawParagraph) Paragraph where
	assignNumbers paraSection (paraNumber, RawParagraph{..}) = do
		nums <- get
		put nums{noteNr=1, exampleNr=1, nextSentenceNr=if paraNumbered then 1 else nextSentenceNr nums}
		paraElems <- assignNumbers paraSection rawParaElems
		when paraNumbered $ modify $ \newnums -> newnums{nextSentenceNr = nextSentenceNr nums}
		return $ assignItemNumbers Paragraph
		  { paraInItemdescr = rawParaInItemdescr
		  , paraSourceLoc = rawParaSourceLoc
		  , allParaElems = allElements paraElems
		  , .. }

treeizeSections :: forall m . (Functor m, MonadFix m, MonadState Numbers m) =>
	Int -> Chapter -> [Section] -> [LinearSection] -> m [Section]
treeizeSections _ _ _ [] = return []
treeizeSections sectionNumber chapter parents
	(s@LinearSection{..} : (span ((> lsectionLevel s) . lsectionLevel) -> (lsubsections, more'))) = mdo
		let
		  ie = rawIndexEntriesForSec newSec
		  newSec = Section
			{ sectionKind = lsectionKind
			, secIndexEntries = ie
			, secIndexEntriesByPath = reverseIndexEntryMap ie
			, sectionName = lsectionName
			, abbreviation = lsectionAbbreviation
			, .. }
		let pn = paraNumbers $ paraNumbered . lsectionParagraphs
		nums <- get
		put nums{itemDeclNr=1}
		sectionFootnotes <- assignNumbers newSec lsectionFootnotes
		modify $ \n -> n{nextSentenceNr=1}
		paragraphs <- forM (zip pn lsectionParagraphs) $ assignNumbers newSec
		subsections <- treeizeSections 1 chapter (newSec : parents) lsubsections
		(newSec :) . treeizeSections (sectionNumber + 1) chapter parents more'

instance AssignNumbers a b => AssignNumbers [a] [b] where
	assignNumbers s = mapM (assignNumbers s)

resolveGrammarterms :: Parser.Macros -> [Text] -> LinearSection -> LinearSection
resolveGrammarterms macros links LinearSection{..} =
	LinearSection{lsectionParagraphs = map resolve lsectionParagraphs, ..}
	where
		resolveTexPara :: RawTexPara -> RawTexPara
		resolveTexPara RawTexPara{..} = RawTexPara{rawTexParaElems =map resolveRawElem rawTexParaElems, ..}
		resolveRawElem (RawBnf s tex) = RawBnf s (bnfGrammarterms macros links tex)
		resolveRawElem y = y
		resolve :: RawParagraph -> RawParagraph
		resolve RawParagraph{..} = RawParagraph{rawParaElems = map resolveTexPara rawParaElems, ..}

bnfGrammarterms :: Parser.Macros -> [Text] -> LaTeX -> LaTeX
bnfGrammarterms macros links = mapTeX go . mapTeX wordify
	where
		wordify :: LaTeXUnit -> Maybe LaTeX
		wordify (TeXRaw stuff) = Just $ map TeXRaw $ unfoldr f stuff
			where
				f s | Text.null s = Nothing
				f s | isName $ Text.head s = Just $ Text.span isName s
				f s = Just $ Text.break isName s

				isName c = isAlpha c || c `elem` ['-', '_']
		wordify _ = Nothing

		go :: LaTeXUnit -> Maybe LaTeX
		go d@(TeXComm cmd _ _) | cmd `elem` ["tcode", "index", "textnormal", "indexlink", "hiddenindexlink", "indexedspan", "renontermdef", "terminal", "literalterminal", "noncxxterminal"] = Just [d]
		go (TeXRaw name)
			| name `elem` links = Just $ fst $ RawDocument.doParse macros $ "\\grammarterm{" ++ name ++ "}"
		go _ = Nothing

parseIndex :: LaTeX -> (IndexPath, Maybe IndexKind)
parseIndex = go . mapTeXRaw unescapeIndexPath . concatRaws
	where
		go (texStripInfix "|seealso" -> Just (x, [TeXBraces y])) = (parseIndexPath x, Just $ See True y)
		go (texStripInfix "|see " -> Just (x, [TeXBraces y])) = (parseIndexPath x, Just $ See False y)
		go (texStripInfix "|see" -> Just (x, [TeXBraces y])) = (parseIndexPath x, Just $ See False y)
		go (texStripInfix "|(" -> Just (t, _)) = (parseIndexPath t, Just IndexOpen)
		go (texStripInfix "|)" -> Just (t, _)) = (parseIndexPath t, Just IndexClose)
		go (texStripInfix "|idxbfpage" -> Just (t, _)) = (parseIndexPath t, Just DefinitionIndexEntry)
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

sectionTexParas :: Section -> [TeXPara]
sectionTexParas s = (paragraphs s >>= paraElems) ++ (sectionFootnotes s >>= footnoteContent)

sectionTex :: Section -> LaTeX
sectionTex s = sectionTexParas s >>= texParaTex

sectionIndexEntries :: Section -> [IndexEntry]
sectionIndexEntries s =
	[ IndexEntry{indexEntrySection=abbreviation sec, ..}
	| sec <- sections s
	, [ (FixArg, [TeXRaw (Text.unpack -> read -> Just -> indexEntryNr)])
	  , (OptArg, [TeXRaw indexCategory]), (FixArg, (parseIndex -> (indexPath, indexEntryKind)))
	  ] <- lookForCommand "index" (sectionTex sec)]

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
				= pre ++ "\\pnum{" ++ file ++ "}{" ++ show lineNr ++ "}" ++ (if null post then "%" else post)
			| otherwise = line

getFileList :: IO [FilePath]
getFileList =
	(\\ ["front", "back"]) .
	map (Text.unpack . Text.dropEnd 1 . Text.drop (Text.length pre)) .
	filter (pre `isPrefixOf`) .
	Text.lines . readFile "std.tex"
  where pre = "\\include{"

grabBnf :: [String] -> [String]
grabBnf [] = []
grabBnf (line : rest)
    | "\\begin{bnf}" `List.isPrefixOf` line =
        let (x, end : more) = break ("\\end{bnf}" `List.isPrefixOf`) rest
        in ["", line] ++ x ++ [end] ++ grabBnf more
    | "\\gramSec" `List.isPrefixOf` line = ["", line] ++ grabBnf rest
    | otherwise = grabBnf rest

generateStdGramExt :: [FilePath] -> IO Text
generateStdGramExt files =
    Text.pack . unlines . grabBnf . lines . Text.unpack .
    Text.concat . mapM readFile ((++ ".tex") . files)

parseFiles :: Parser.Macros -> IO ([[LinearSection]], Parser.Macros)
parseFiles m = do
	files <- getFileList
	stdGramExt <- generateStdGramExt files
	let
		go [] macros = return ([], macros)
		go (c:cc) macros = do
			let p = c ++ ".tex"

			stuff <-
				replace "multicolfloattable" "floattable" .
				replace "\\indeximpldef{" "\\index[impldefindex]{" .
				Text.unlines .
					indexCodeEnvs ["codeblock", "itemdecl"] .
					moveIndexEntriesIntoSecs .
					moveIndexEntriesIntoDefs .
				Text.lines .
				trackPnums p .
				replace "\\nodiffref\n\\change" "\n\\pnum\\textbf{Change:}\\space" .
				replace "\n\\diffref" "\n\\pnum\\nopnumdiffref" .
					-- Done here because (1) the real \nodiffref is defined with \def in a way
					-- we don't support yet, and (2) this way a source link is generated for the pnum.
				readFile p

			let extra = if c /= "grammar" then "" else replace "\\gramSec" "\\rSec1" stdGramExt
			let (r, macros') = parseFile macros (stuff ++ extra)
			if length r == 0 then undefined else
				first (r:) . go cc (macros ++ macros')
	go files m

load14882 :: Text -> IO Draft
load14882 extraMacros = do

	commitUrl <- getCommitUrl

	(macros@Parser.Macros{..}, took) <- measure (loadMacros extraMacros)
	putStrLn $ "Loaded macros in " ++ show (took * 1000) ++ "ms."

	(secs :: [LinearSection], took2) <- measure $ mconcat . fst . parseFiles macros
	putStrLn $ "Parsed LaTeX in " ++ show (took2 * 1000) ++ "ms."

	xrefDelta <- loadXrefDelta

	(r, took3) <- measure $ if length (show secs) == 0 then undefined else do
		-- force eval before we leave the dir
		let
			grammarNames = [n |
				TeXComm "index" _ [
					(OptArg, [TeXRaw "grammarindex"]) ,
					(FixArg, [TeXRaw _
					 ,TeXComm "textcolor" "" [(FixArg,[TeXRaw "grammar-gray"]),(FixArg,[TeXComm "textsf" _ [(FixArg,[TeXComm "textit" "" [(FixArg,[TeXRaw n])]])]])]
					 ,TeXRaw "|idxbfpage"]
					)] <- allUnits secs]

			secs' = map (resolveGrammarterms macros grammarNames) secs
			chapters = evalState (treeizeChapters False 1 secs') (Numbers 1 1 1 1 0 0 1 1 1)
			allEntries :: [IndexEntry]
			allEntries = chapters >>= sectionIndexEntries
			index = mergeIndices $ map toIndex allEntries
			indexEntryMap = IntMap.fromList [(n, e) | e@IndexEntry{indexEntryNr=Just n} <- allEntries]
			indexEntriesByPath = reverseIndexEntryMap indexEntryMap
	
			abbrMap = makeAbbrMap dr
			dr = Draft{..}
		return dr
	
	putStrLn $ "Processed in " ++ show (took3 * 1000) ++ "ms."
	return r
