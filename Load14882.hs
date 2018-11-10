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
import qualified Data.List as List
import Data.IntMap (IntMap)
import LaTeXBase
	( LaTeXUnit(..), LaTeX, TeXArg, ArgKind(..), lookForCommand
	, mapTeX, mapTeXRaw, concatRaws, texStripInfix, allUnits)
import Data.Text (Text, replace, isPrefixOf, isSuffixOf)
import Data.Text.IO (readFile)
import qualified Data.Text as Text
import Control.Monad (forM)
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (isAlpha, isSpace, isDigit)
import Control.Arrow (first)
import Data.Map (Map, keys)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Data.List (sort, unfoldr, (\\))
import System.Process (readProcess)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState, evalState, get, put, liftM2, modify)
import Util ((.), (++), mapLast, stripInfix, textStripInfix)
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

splitIntoSentences :: [RawElement] -> [[RawElement]]
splitIntoSentences = go []
	where
		go [] [] = []
		go [] (RawLatexElement (TeXRaw "\n") : y) = go [] y
		go [] (x@(RawExample _) : y) = [x] : go [] y
		go [] (x@(RawNote _) : y) = [x] : go [] y
		go [] (x@(RawCodeblock _) : y) = [x] : go [] y
		go x [] = [x]
		go x z@(e : y)
			| Just (s, rest) <- breakSentence z = (x ++ s) : go [] rest
			| otherwise = go (x ++ [e]) y
		breakSentence :: [RawElement] ->
			Maybe ([RawElement] {- sentence -}, [RawElement] {- remainder -})
		breakSentence (RawLatexElement (TeXRaw x) : more)
			| Just ((++ ".") -> pre, post) <- textStripInfix "." x
			, not (("(." `isSuffixOf` pre) && (")" `isPrefixOf` post))
			, not (("e." `isSuffixOf` pre) && ("g." `isPrefixOf` post))
			, not (("i." `isSuffixOf` pre) && ("e." `isPrefixOf` post))
			, not (Text.length pre > 1 && Text.length post > 0 && isDigit (Text.last $ Text.init pre) && isDigit (Text.head post))
			, not ("e.g." `isSuffixOf` pre)
			, not ("i.e." `isSuffixOf` pre) =
				let
					post' = Text.dropWhile isSpace post
					(pre', post'') = case Text.stripPrefix ")" post' of
						Just z -> (pre ++ ")" , Text.dropWhile isSpace z)
						Nothing -> (pre, post')
					more' = if post'' == "" then more else RawLatexElement (TeXRaw post'') : more
					(maybefootnote, more'') = case more' of
						fn@(RawLatexElement (TeXComm "footnoteref" _)) : z -> ([fn], z)
						_ -> ([], more')
					sentence = [RawLatexElement (TeXRaw pre')] ++ maybefootnote
				in
					Just (sentence, more'')
		breakSentence (enum@(RawEnumerated _ (last -> rawItemContent -> last -> RawTexPara (last -> el))) : more)
			| Just _ <- breakSentence [el] = Just ([enum], more)
		breakSentence _ = Nothing

isActualSentence :: [RawElement] -> Bool
isActualSentence = any p
	where
		yes = words $
			"link tcode textit ref grammarterm indexedspan " ++
			"defnx textbf textrm textsl textsc deflinkx"

		q :: LaTeXUnit -> Bool
		q (TeXRaw s) = not $ all isSpace $ Text.unpack s
		q (TeXComm c _) | c `elem` yes = True
		q (TeXEnv c _ _) | c `elem` yes = True
		q (TeXEnv "indexed" _ body) = any q body
		q (TeXBraces body) = any q body
		q _ = False

		p :: RawElement -> Bool
		p (RawLatexElement u) = q u
		p RawEnumerated{} = True
		p _ = False

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

instance AssignNumbers RawItem Item where
	assignNumbers s (RawItem label content) = do
		n <- get
		put n{nextSentenceNr = 1}
		Item Nothing (if null label then Nothing else Just label) . assignNumbers s content
	
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
	assignNumbers s (RawEnumerated x p) = do
		origNum <- nextSentenceNr . get
		r <- Enumerated x . mapM (assignNumbers s) p
		modify $ \y -> y{nextSentenceNr = origNum}
		return r
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

mapTexPara :: ([Element] -> [Element]) -> (TeXPara -> TeXPara)
mapTexPara f (TeXPara x) = TeXPara (map g x)
	where
		g :: Sentence -> Sentence
		g s = s{sentenceElems = f (sentenceElems s)}

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
						(fst $ goParas (mapLast (+i) nn ++ [1]) itemContent)
					) (zip [0..] enumItems)
		goElem nn (NoteElement (Note nr paras)) = (NoteElement (Note nr paras'), nn')
			where (paras', nn') = goParas nn paras
		goElem nn x = (x, nn)

instance AssignNumbers (Maybe Int, RawParagraph) Paragraph where
	assignNumbers paraSection (paraNumber, RawParagraph{..}) = do
		nums <- get
		put nums{noteNr=1, exampleNr=1, nextSentenceNr=1}
		paraElems <- assignNumbers paraSection rawParaElems
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
		let newSec = Section
			{ sectionKind = lsectionKind
			, secIndexEntries = rawIndexEntriesForSec newSec
			, sectionName = lsectionName
			, abbreviation = lsectionAbbreviation
			, .. }
		let pn = paraNumbers $ paraNumbered . lsectionParagraphs
		nums <- get
		put nums{itemDeclNr=1}
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
	((Map.fromList $ map (, s) (paragraphs >>= paraElems >>= texParaElems >>= nontermdefsInElement))
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
		paragraphs  = map (\p -> p{paraElems = map (mapTexPara (map resolve)) (paraElems p)}) paragraphs,
		subsections = map (resolveGrammarterms links) subsections,
		sectionFootnotes = map resolveFN sectionFootnotes,
		..}
	where
		resolveParas = map $ mapTexPara $ map resolve
		resolveFN :: Footnote -> Footnote
		resolveFN fn@Footnote{..} = fn{footnoteContent = resolveParas footnoteContent}
		resolve :: Element -> Element
		resolve (LatexElement e) = LatexElement $ head $ grammarterms links [e]
		resolve (Enumerated s ps) = Enumerated s $ map f ps
			where f i@Item{..} = i{itemContent = resolveParas itemContent}
		resolve (Bnf n b) = Bnf n $ grammarterms links $ bnfGrammarterms links b
		resolve (NoteElement n@Note{..}) = NoteElement n{noteContent = resolveParas noteContent}
		resolve (ExampleElement e@Example{..}) = ExampleElement e{exampleContent = resolveParas exampleContent}
		resolve other = other

grammarterms :: GrammarLinks -> LaTeX -> LaTeX
grammarterms links = mapTeX (go links)
	where
		go g (TeXComm "grammarterm" args@((FixArg, [TeXRaw name]) : _))
			| Just Section{..} <- Map.lookup (Text.toLower name) g =
				Just [TeXComm "grammarterm_" ((FixArg, abbreviation) : args)]
		go _ _ = Nothing

bnfGrammarterms :: GrammarLinks -> LaTeX -> LaTeX
bnfGrammarterms links = mapTeX go . mapTeX wordify
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
		go d@(TeXComm "nontermdef" _) = Just [d]
		go n@(TeXRaw name)
			| Just Section{..} <- Map.lookup name links =
				Just [TeXComm "grammarterm_" [(FixArg, abbreviation), (FixArg, [n])]]
		go _ = Nothing

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

sectionTexParas :: Section -> [TeXPara]
sectionTexParas s = (paragraphs s >>= paraElems) ++ (sectionFootnotes s >>= footnoteContent)

sectionTex :: Section -> LaTeX
sectionTex s = sectionTexParas s >>= texParaTex

sectionIndexEntries :: Section -> [IndexEntry]
sectionIndexEntries s =
	[ IndexEntry{isDefinitionIndexEntry = False, ..}
	| indexEntrySection <- sections s
	, [(FixArg, [TeXRaw (Text.unpack -> read -> Just -> indexEntryNr)]), (OptArg, [TeXRaw indexCategory]), (FixArg, (parseIndex -> (indexPath, indexEntryKind)))]
		<- lookForCommand "index" (sectionTex indexEntrySection)] ++
	[ IndexEntry
		{ indexCategory = "generalindex"
		, isDefinitionIndexEntry = True
		, ..}
	| indexEntrySection <- sections s
	, [(FixArg, [TeXRaw (Text.unpack -> read -> Just -> indexEntryNr)]), (FixArg, _), (FixArg, (parseIndex -> (indexPath, indexEntryKind)))]
		<- lookForCommand "defnx" (sectionTex indexEntrySection)]

toIndex :: IndexEntry -> Index
toIndex IndexEntry{..} = Map.singleton indexCategory $ go indexPath
	where
		go :: [IndexComponent] -> IndexTree
		go [c] = Map.singleton c (IndexNode [IndexEntry indexEntrySection indexEntryKind indexPath indexEntryNr indexCategory isDefinitionIndexEntry] Map.empty)
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

load14882 :: IO Draft
load14882 = do

	commitUrl <- getCommitUrl

	macros@Parser.Macros{..} <- loadMacros

	putStrLn $ ("Loaded macros: " ++) $ unwords $ sort $
		keys commands ++ (Text.unpack . keys environments)

	files <- getFileList
	stdGramExt <- generateStdGramExt files

	putStrLn "Loading chapters"
	secs <- forM files $ \c -> do
		let p = c ++ ".tex"
		putStr $ "  " ++ c ++ "... "; hFlush stdout

		stuff <-
			replace "multicolfloattable" "floattable" .
			replace "\\indeximpldef{" "\\index[impldefindex]{" .
			moveIndexEntriesIntoDefs .
			trackPnums p .
			replace "\\nodiffref\n\\change" "\n\\pnum\\textbf{Change:}\\space" .
			replace "\n\\diffref" "\n\\pnum\\nopnumdiffref" .
				-- Done here because (1) the real \nodiffref is defined with \def in a way
				-- we don't support yet, and (2) this way a source link is generated for the pnum.
			replace "\\makebox[0pt][l]" "" . -- handled here because we don't support multiple optional args yet
			readFile p

		let extra = if c /= "grammar" then "" else replace "\\gramSec" "\\rSec1" stdGramExt
		let r = parseFile macros (stuff ++ extra)

		putStrLn $ show (length r) ++ " sections"
		return r

	xrefDelta <- loadXrefDelta

	if length (show secs) == 0 then undefined else do
		-- force eval before we leave the dir
		let
			chapters = evalState (treeizeChapters False 1 $ mconcat secs) (Numbers 1 1 1 1 0 0 1 1 1)
			ntdefs = Map.unions $ map nontermdefsInSection chapters
			chapters' = map (resolveGrammarterms ntdefs) chapters
			allEntries :: [IndexEntry]
			allEntries = chapters' >>= sectionIndexEntries
			index = mergeIndices $ map toIndex allEntries
			indexEntryMap = IntMap.fromList [(n, e) | e@IndexEntry{indexEntryNr=Just n} <- allEntries]

		return Draft{chapters=chapters', ..}
