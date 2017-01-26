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

import Document
import qualified Text.LaTeX.Base.Render as TeXRender
import qualified LaTeXParser as Parser
import Text.LaTeX.Base (protectString)
import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), lookForCommand, matchEnv, matchCommand, (<>))
import Data.Text (Text, replace, isPrefixOf)
import Data.Text.IO (readFile)
import qualified Data.Text as Text
import Control.Monad (forM)
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (isSpace, isAlpha)
import Control.Arrow (first)
import Data.Map (Map, keys)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Data.List (sort, unfoldr, take)
import Data.Maybe (isJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Text.Regex (mkRegex, subRegex)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState, evalState, get, put, liftM2)
import Util ((.), (++), mapLast, mapHead, stripInfix)
import LaTeXUtil (texFromArg, mapTeXArg, mapTeXRaw, texTail, concatRaws, mapTeX, rmseqs, texStripPrefix, texStripInfix)
import LaTeXParser (Macros(..), Command, Environment, Signature(..))


signatures :: [(String, Signature)]
signatures =
		[(c, Signature i Nothing) | i <- [0..3], c <- words (a i)] ++
		[ ("\n", Signature 0 Nothing)
		, ("index", Signature 2 (Just []))
		, ("caption", Signature 2 (Just []))
		, ("gramSec", Signature 2 (Just []))
		]
	where
		a 0 = "today item def makeatletter bottomline makeatother Sec left right bmod " ++
			"chapter section paragraph subparagraph fi otextup linebreak newpage log kill " ++
			"textup edef x itcorrwidth itletterwidth small BnfIndent setlength par leq " ++
			"leftmargini BnfInc BnfRest kern protect textsmaller caret sum clearpage " ++
			"xspace onelineskip textlangle textrangle textunderscore tilde raggedright = " ++
			"space copyright textregistered textbackslash hsize makebox nocorr br Gamma " ++
			"frenchspacing list leftmargin listparindent itemindent rmfamily itshape relax " ++
			"color nonfrenchspacing endlist upshape ttfamily baselineskip nobreak noindent " ++
			"endfirsthead quad cdot cdots dotsc bnfindentinc footnotemark ldots capsep max min " ++
			"continuedcaption hline endhead footnotesize le times dotsb rightarrow to equiv " ++
			"lfloor rfloor pi geq neq ge lceil rceil ell alpha bigl bigr mu lambda beta " ++
			"tabularnewline exp sigma big delta rho Pi nu infty displaystyle lim sin cos " ++
			"phi int theta zeta FlushAndPrintGrammar hfill break backslash"
		a 1 = "hspace footnote textit textrm textnormal texttt textbf ensuremath ref mbox " ++
			"terminal enlargethispage nontermdef textsl textsc text grammarterm term " ++
			"tcode descr footnotetext microtypesetup cline mathtt mathit mathrm mathsf " ++
			"newcolumntype label newlength uline vspace value newcounter mathscr " ++
			"phantom sqrt ln emph lstset"
		a 2 = "pnum addtolength definition defnx addtocounter setcounter frac " ++
			"glossary binom infannex normannex parbox definitionx"
		a 3 = "multicolumn discretionary definecolor"

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

assignItemNumbers :: Paragraph -> Paragraph
assignItemNumbers p
	| Just n <- paraNumber p = p{ paraElems = fst $ goElems [n,1] (paraElems p) }
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
							(fst (goElems (mapLast (+i) nn ++ [1]) itemContent))
						) (zip [0..] enumItems)
				in
					first (Enumerated enumCmd items' :) (goElems (mapLast (+ length enumItems) nn) ee)
			_ -> first (e:) (goElems nn ee)

-- We don't represent examples as elements with nested content
-- because sometimes they span multiple (numbered) paragraphs.

type RawElements = [RawElement]

data RawParagraph = RawParagraph
	{ paraNumbered :: Bool
	, rawParaInItemdescr :: Bool
	, rawParaElems :: RawElements
	, rawParaSourceLoc :: Maybe SourceLocation }
	deriving Show

data LinearSection = LinearSection
	{ lsectionAbbreviation :: LaTeX
	, lsectionKind :: SectionKind
	, lsectionName :: LaTeX
	, lsectionParagraphs :: [RawParagraph] }
	deriving Show

isEnumerate :: LaTeX -> Maybe String
isEnumerate (TeXEnv s _ _)
	| s `elem` ["enumeratea", "enumerate", "itemize", "description"] = Just s
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
isParaEnd (TeXEnv "itemdecl" _ _) = True
isParaEnd (TeXEnv "itemdescr" _ _) = True
isParaEnd (TeXComm "pnum" _) = True
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

parseItems :: [LaTeX] -> [RawElements]
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
	go e' : parsePara more ++ (RawFootnote . fnotes)
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
			| k == "itemdecl" = RawLatexElements [e]
		go other = error $ "parsePara: unexpected " ++ show other

		(e', fnotes) = extractFootnotes env

parsePara (elems -> (extractFootnotes -> (e, fnotes), more))
	= RawLatexElements e : parsePara more ++ (RawFootnote . fnotes)

elems :: [LaTeX] -> ([LaTeX], [LaTeX])
elems [] = ([], [])
elems y@(x:xs)
	| isElementsEnd x = ([], y)
	| TeXRaw (Text.breakOn "\n\n" -> (a, b)) <- x
	, b /= "" = ([TeXRaw a], TeXRaw (Text.drop 2 b) : xs)
	| otherwise = first (x :) (elems xs)

parseParas :: [LaTeX] -> ([RawParagraph], [LaTeX] {- rest -})
parseParas (break isParasEnd -> (extractFootnotes -> (stuff, fs), rest))
		= (collectParas stuff ++ [RawParagraph False False (RawFootnote . fs) Nothing], rest)
	where
		collectParas :: [LaTeX] -> [RawParagraph]
		collectParas (t@(TeXEnv "itemdecl" _ _) : more) =
			RawParagraph False False (parsePara [t]) Nothing : collectParas more
		collectParas (TeXEnv "itemdescr" _ desc : more) =
			map (\p -> p{rawParaInItemdescr=True}) (collectParas $ rmseqs desc)
			++ collectParas more
		collectParas (TeXComm "pnum"
			[ FixArg (TeXRaw (Text.unpack -> file))
			, FixArg (TeXRaw (Text.unpack -> read -> lineNr))] : more) =
				(\(p : x) -> p{paraNumbered=True, rawParaSourceLoc=Just (SourceLocation file lineNr)} : x)
				(collectParas more)
		collectParas [] = []
		collectParas x = (RawParagraph False False (parsePara p) Nothing : ps)
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
		("rSec", [FixArg (TeXRaw level), FixArg abbr, FixArg name]) ->
			(abbr, name, NormalSection $ read $ Text.unpack level)
		("definition", [FixArg name, FixArg abbr]) ->
			(abbr, name, DefinitionSection 2)
		("definitionx", [FixArg name, FixArg abbr]) ->
			(abbr, name, DefinitionSection 3)
		_ -> error $ "not a section command: " ++ show c
	= LinearSection{..} : moreSections
parseSections [] = []
parseSections (x:_) = error $ "parseSections: " ++ show x

initialContext :: Parser.Context
initialContext = Parser.defaultContext
	{ Parser.dontEval = (bnfEnvs ++) $ words $
			"drawing definition definitionx importgraphic bottomline capsep itemdescr " ++
			"grammarterm nontermdef defnx FlushAndPrintGrammar term caret indented " ++
			"tabular longtable enumeratea emph"
	, Parser.kill = ["clearpage", "enlargethispage", "noindent",
			"indent", "vfill", "pagebreak", "!", "-", "glossary",
			"itcorr", "hfill", "space", "nocorr", "small", "kill", "lstset",
			"footnotesize", "rmfamily", "microtypesetup", "@", "ungap", "gramSec", "newcolumntype"]
	, Parser.signatures = signatures }

doParse :: Macros -> Text -> (LaTeX, Macros)
doParse m t = (x, y)
	where
		(x, y, []) = Parser.parseString ctx (Text.unpack t)
		ctx = initialContext{Parser.macros=m}

parseFile :: Macros -> Text -> [LinearSection]
parseFile macros =
	parseSections
	. filter (not . isComment)
	. rmseqs . fst
	. doParse macros
	. replace "$$" "$"
	. replace "\\hspace*" "\\hspace"
	. replace "``" "“"
	. replace "''" "”"

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
		return $ FootnoteElement Footnote{footnoteNumber=footnoteNr,footnoteContent=t'}
	assignNumbers s (RawEnumerated x p) = Enumerated x . (Item Nothing .) . assignNumbers s p
	assignNumbers s (RawLatexElements x) = LatexElements . assignNumbers s (filter (/= TeXRaw "") x)
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
		let newSec = Section{..}
		let pn = paraNumbers $ paraNumbered . lsectionParagraphs
		paragraphs <- forM (zip pn lsectionParagraphs) $
			\(paraNumber, RawParagraph{..}) -> do
				paraElems <- assignNumbers newSec rawParaElems
				let paraSection = newSec
				let paraSourceLoc = rawParaSourceLoc
				return $ assignItemNumbers $ Paragraph{paraInItemdescr = rawParaInItemdescr, ..}
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
		let newSec = Section{..}
		let pn = paraNumbers $ paraNumbered . lsectionParagraphs
		paragraphs <- forM (zip pn lsectionParagraphs) $
			\(paraNumber, RawParagraph{..}) -> do
				paraElems <- assignNumbers newSec rawParaElems
				let paraSection = newSec
				let paraSourceLoc = rawParaSourceLoc
				return $ assignItemNumbers $ Paragraph{paraInItemdescr = rawParaInItemdescr, ..}
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
		resolve g (Enumerated s ps) = Enumerated s $ map f ps
			where f i@Item{..} = i{itemContent=map (resolve g) itemContent}
		resolve g (Bnf n b) = Bnf n $ bnfGrammarterms g b
		resolve g (FootnoteElement (Footnote n c)) = FootnoteElement (Footnote n $ map (resolve g) c)
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

parseIndex :: LaTeX -> (IndexPath, Maybe IndexKind)
parseIndex = go . mapTeXRaw unescapeIndexPath . concatRaws

	where
		go (texStripInfix "|seealso" -> Just (x, y)) = (parseIndexPath x, Just $ See True y)
		go (texStripInfix "|see" -> Just (x, y)) = (parseIndexPath x, Just $ See False y)
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

sectionIndexEntries :: Section -> [RawIndexEntry]
sectionIndexEntries s =
	[ RawIndexEntry{..}
	| indexSection <- sections s
	, [OptArg (TeXRaw indexCategory), FixArg (parseIndex -> (rawIndexPath, rawIndexKind))]
		<- paragraphs indexSection >>= paraElems >>= elemTex >>= lookForCommand "index" ] ++
	[ RawIndexEntry
		{ indexCategory = "generalindex"
		, rawIndexKind = Just DefinitionIndex
		, ..}
	| indexSection <- sections s
	, [FixArg _, FixArg (parseIndex -> (rawIndexPath, Nothing))]
		<- paragraphs indexSection >>= paraElems >>= elemTex >>= lookForCommand "defnx" ]

toIndex :: RawIndexEntry -> Index
toIndex RawIndexEntry{..} = Map.singleton indexCategory $ go rawIndexPath
	where
		go :: [IndexComponent] -> IndexTree
		go [c] = Map.singleton c (IndexNode [IndexEntry indexSection rawIndexKind rawIndexPath] Map.empty)
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

	macros@Macros{..} <-
		snd
		. doParse mempty
		. replace "\\indeximpldef{" "\\index[impldefindex]{"
		. Text.pack . flip (subRegex (mkRegex "\\\\penalty[0-9]+")) "" . Text.unpack
		. ("\\newcommand{\\texorpdfstring}[2]{#2}\n" ++)
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
		let chapters = evalState (treeizeChapters 1 $ mconcat secs) (Numbers 1 1 1 1)

		let ntdefs = Map.unions $ map nontermdefsInSection chapters
		let chapters' = map (resolveGrammarterms ntdefs) chapters
		let index = mergeIndices $ map toIndex (chapters' >>= sectionIndexEntries)

		return Draft{chapters=chapters', ..}
