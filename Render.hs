{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	TupleSections,
	ViewPatterns,
	LambdaCase,
	TypeSynonymInstances,
	FlexibleInstances #-}

module Render (
	Render(render), url, renderTab, renderFig, simpleRender, squareAbbr,
	linkToSection, secnum, SectionFileStyle(..), applySectionFileStyle,
	fileContent, Link(..), outputDir, linkToRemoteTable, defaultRenderContext,
	abbrAsPath, abbreviations, RenderContext(..),
	) where

import Load14882 (parseIndex) -- todo: bad
import Document (
	CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Draft(..), Footnote(..),
	Section(..), Chapter(..), Table(..), Figure(..), figures, tables, Item(..),
	IndexComponent(..), IndexTree, IndexNode(..), IndexKind(..), IndexEntry(..),
	IndexPath, indexKeyContent, tableByAbbr, figureByAbbr, Paragraph(..))
import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), MathType(..), matchCommand, matchEnv, lookForCommand)
import qualified Data.IntMap as IntMap
import qualified Text.LaTeX.Base.Render as TeXRender
import Data.Text (isPrefixOf)
import qualified Data.Text as Text
import Data.Char (isAlpha, isSpace)
import Control.Monad (liftM2)
import qualified Prelude
import Prelude hiding (take, last, (.), (++), writeFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Text.Regex (mkRegex, subRegex)
import Data.MemoTrie (memo2)
import Data.List (find, nub)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Util ((.), (++), replace, Text, xml, spanTag, anchor, Anchor(..), greekAlphabet, dropTrailingWs, urlChars)
import LaTeXUtil (texFromArg, trim, trimr, needsSpace, mapTeXArg, isMath, isCodeblock, texStripPrefix)

kill, literal :: [String]
kill = words $
	"clearpage renewcommand newcommand enlargethispage noindent indent vfill pagebreak " ++
	"caption capsep continuedcaption bottomline hline rowsep hspace endlist cline itcorr " ++
	"hfill nocorr small endhead kill footnotesize rmfamily microtypesetup nobreak " ++
	"label topline FlushAndPrintGrammar left right protect = ! @ -"
literal = ["#", "{", "}", "~", "%", ""]

simpleMacros :: [(String, Text)]
simpleMacros =
	[ ("dcr"            , "--")
	, (","              , "<span style='white-space:nowrap'>&thinsp;</span>")
	                           -- thin, non-breaking, non-stretching space
	, ("\""             , "\"")
	, ("prime"          , "'")
	, ("caret"          , "^")
	, ("copyright"      , "&copy;")
	, ("textregistered" , "&reg;")
	, ("Cpp"            , "C++")
	, ("cppver"         , "201402L")
	, ("sum"            , "∑")
	, ("ell"            , "ℓ")
	, ("shr"            , ">>")
	, ("cv"             , "cv")
	, ("shl"            , "&lt;&lt;")
	, ("br"             , "<br/>")
	, ("linebreak"      , "<br/>")
	, ("sim"            , "~")
	, ("quad"           , "&emsp;&ensp;")
	, ("indent"         , "&emsp;")
	, ("unun"           , "__")
	, ("^"              , "^")
	, ("ldots"          , "&hellip;")
	, ("dotsc"          , "&hellip;")
	, ("times"          , "&times;")
	, ("&"              , "&amp;")
	, ("$"              , "&#36;")
	, ("backslash"      , "\\")
	, ("textbackslash"  , "\\")
	, ("textunderscore" , "_")
	, ("colcol"         , "::")
	, ("tilde"          , "~")
	, ("hspace"         , " ")
	, ("space"          , " ")
	, ("equiv"          , "&equiv;")
	, ("le"             , "≤")
	, ("leq"            , "≤")
	, ("ge"             , "≥")
	, ("geq"            , "≥")
	, ("neq"            , "≠")
	, ("cdot"           , "·")
	, ("cdots"          , "⋯")
	, ("to"             , "→")
	, ("rightarrow"     , "→")
	, ("sqrt"           , "√")
	, ("lfloor"         , "⌊")
	, ("rfloor"         , "⌋")
	, ("lceil"          , "⌈")
	, ("rceil"          , "⌉")
	, (";"              , " ")
	, ("min"            , "<span class=\"mathrm\">min</span>")
	, ("max"            , "<span class=\"mathrm\">max</span>")
	, ("bmod"           , "<span class=\"mathrm\">mod</span>")
	, ("exp"            , "<span class=\"mathrm\">exp</span>")
	, ("ln"             , "<span class=\"mathrm\">ln</span>")
	, ("log"            , "<span class=\"mathrm\">log</span>")
	, ("opt"            , "<sub><small>opt</small></sub>")
	, ("rightshift"     , "<span class=\"mathsf\">rshift</span>")
	, ("textlangle"     , "&langle;")
	, ("textrangle"     , "&rangle;")
	, (">"              , "&#9;")
	, ("\n"             , "\n")
	]
	++ [(n, Text.pack [c]) | (n, c) <- greekAlphabet]

zwsp :: Text
zwsp = "&#x200b;" -- U+200B ZERO WIDTH SPACE

makeSpan, makeDiv, makeBnfTable, makeBnfPre :: [String]
makeSpan = words "center grammarterm mbox mathsf emph terminal textsc phantom term mathtt textnormal textrm descr textsl textit mathit indented"
makeDiv = words "definition cvqual emph exitnote footnote mathit paras ttfamily TableBase table tabular longtable"
makeBnfTable = words "bnfkeywordtab bnftab ncbnftab"
makeBnfPre = words "bnf ncbnf simplebnf ncsimplebnf"

indexPathString :: IndexPath -> Text
indexPathString =
	replace " " "_" . -- HTML forbids space.
	Text.intercalate "," .
	map (indexKeyContent . indexKey)

indexPathId :: Text -> IndexPath -> Text
indexPathId category =
	(if category == "libraryindex" then ("lib" ++) else id) .
	(":" ++) .
	replace " "  "%20" .
	replace "'" "&#39;" .
	indexPathString

indexPathHref :: IndexPath -> Text
indexPathHref = (":" ++) . urlChars . indexPathString

asId :: LaTeX -> Text
asId (TeXRaw t) = replace "\n" "_" $ replace " " "_" t
asId (TeXSeq x y) = asId x ++ asId y
asId TeXEmpty = ""
asId (TeXComm "tcode" [FixArg x]) = asId x
asId (TeXComm "texttt" [FixArg x]) = asId x
asId (TeXComm "textit" [FixArg x]) = asId x
asId (TeXComm "mathsf" [FixArg x]) = asId x
asId (TeXCommS "xspace") = "_"
asId (TeXBraces x) = asId x
asId (TeXMath Dollar x) = asId x
asId (TeXComm "texorpdfstring" [_, FixArg x]) = asId x
asId x = error $ "asId: unexpected: " ++ show x

instance Render Anchor where
	render Anchor{..} _ = xml "a" ([("class", aClass) | aClass /= "" ] ++
	                             [("href" , aHref ) | aHref  /= "" ] ++
	                             [("id"   , aId   ) | aId    /= "" ] ++
	                             [("style", aStyle) | aStyle /= "" ])
	                        aText

class Render a where render :: a -> RenderContext -> Text

instance Render a => Render [a] where
	render = mconcat . map render

instance Render Char where render c _ = Text.pack [c]

instance (Render a, Render b) => Render (a, b) where
	render (x, y) = render x ++ render y

instance Render TeXArg where
	render = render . texFromArg

rmClause :: LaTeX -> LaTeX
rmClause (TeXSeq (TeXRaw "Clause~") x) = x
rmClause (TeXSeq (TeXRaw "Clause ") x) = x
rmClause (TeXSeq (TeXRaw "Table~") x) = x
rmClause (TeXSeq (TeXRaw "Annex~") x) = x
rmClause x = x

redundantOpen :: Text -> Bool
redundantOpen (Text.unpack -> (c:'(':s))
	= (c `elem` ("~ \n" :: String))
	&& (s `elem` ["", "Clause ", "Clause~"])
redundantOpen _ = False

renderCodeblock :: LaTeX -> RenderContext -> Text
renderCodeblock env@(TeXEnv _ _ t) = \c -> xml "pre" [("class", "codeblock")]
	(render (trimr t) c{rawTilde=True, rawHyphens=True, rawSpace=True, inCodeBlock=True})

sameIdNamespace :: Maybe IndexKind -> Maybe IndexKind -> Bool
sameIdNamespace Nothing (Just IndexOpen) = True
sameIdNamespace (Just IndexOpen) Nothing = True
sameIdNamespace x y = x == y

indexOccurrenceSuffix :: RenderContext -> Int -> Text
	-- Returns the _ that distinguishes expr#def:object_expression from
	-- expr#def:object_expression_ ([expr] has two definitions of 'object expression',
	-- one for E1.E2 and one for E1.*E2.)
indexOccurrenceSuffix c indexNum
	| Nothing <- page c, Nothing <- draft c = ""
	| otherwise = Text.pack $ replicate numPre '_'
	where
		m	| Just s <- page c = secIndexEntries s
			| Just d <- draft c = indexEntryMap d
		(pre, Just theEntry, post) = IntMap.splitLookup indexNum m
		thePath = indexPath theEntry
		p e = indexPath e == indexPath theEntry &&
			indexCategory e == indexCategory theEntry &&
			sameIdNamespace (indexEntryKind e) (indexEntryKind theEntry)
		numPre = IntMap.size $ IntMap.filter p pre

instance Render LaTeX where

	render (TeXSeq gt@(TeXComm "grammarterm_" [FixArg (TeXRaw termSec),  _])
	       (TeXSeq ps@(TeXComm "textit" [FixArg (TeXRaw "s")])
	       (TeXSeq (TeXRaw openParen)
	       (TeXSeq (TeXComm "ref" [FixArg (TeXRaw refSec)])
	       (texStripPrefix ")" -> Just rest)))))
		| refSec == termSec
		, redundantOpen openParen
		= render (TeXSeq gt (TeXSeq ps rest))

	render (TeXSeq gt@(TeXComm "grammarterm_" [FixArg (TeXRaw termSec),  _])
	       (TeXSeq (TeXRaw openParen)
	       (TeXSeq (TeXComm "ref" [FixArg (TeXRaw refSec)])
	       (texStripPrefix ")" -> Just rest))))
		| refSec == termSec
		, redundantOpen openParen
		= render (TeXSeq gt rest)

	render (TeXSeq (TeXCommS "xspace") x) = (if needsSpace x then (" " ++) else id) . render x
	render (TeXSeq (TeXCommS "textbackslash") y)
		| TeXSeq (TeXRaw s) rest <- y  = \sec -> "\\" ++ render (TeXRaw $ if rawSpace sec then s else unspace s) sec ++ render rest sec
		| TeXRaw s <- y                = \sec -> "\\" ++ render (TeXRaw $ if rawSpace sec then s else unspace s) sec
		where
			unspace s
				| Just (c, cc) <- Text.uncons s, isSpace c = cc
				| otherwise = s
	render (TeXSeq (TeXCommS "itshape") x) = ("<i>" ++) . (++ "</i>") . render x
	render (TeXSeq x y               ) = liftM2 (++) (render x) (render y)
	render (TeXRaw x                 ) = \ctx ->
	                                     (if rawHyphens ctx then id
	                                         else replace "--" "–" . replace "---" "—")
	                                   $ (if rawTilde ctx then id else replace "~" " ")
	                                   $ (if insertBreaks ctx then
	                                         replace "::" (zwsp ++ "::" ++ zwsp) .
	                                           -- The LaTeX sources very often neglect to use \colcol.
	                                         replace "_" "_&shy;"
	                                      else id)
	                                   $ (if replXmlChars ctx then
	                                         replace ">" "&gt;" .
	                                         replace "<" "&lt;" .
	                                         replace "&" "&amp;"
	                                      else id)
	                                   $ x
	render (TeXComment _             ) = return ""
	render (TeXCommS "br"            ) = return "<br/>"
	render (TeXLineBreak _ _         ) = return "<br/>"
	render (TeXCommS "break"         ) = return "<br/>"
	render (TeXEmpty                 ) = return ""
	render (TeXBraces t              ) = render t
	render m@(TeXMath _ _            ) = renderMath m
	render (TeXComm "comment" [FixArg comment]) = \c -> spanTag "comment" $ render comment c{rawTilde=False, rawHyphens=False}
	render (TeXComm "ensuremath" [FixArg x]) = renderMath x
	render (TeXComm "ref" [FixArg abbr]) = \ctx ->
		simpleRender anchor{aHref = abbrHref abbr ctx, aText = linkText abbr ctx}
		where
			linkText :: LaTeX -> RenderContext -> Text
			linkText abbr RenderContext{..}
				| "tab:" `isPrefixOf` simpleRender abbr
				, Just Table{..} <- tableByAbbr (fromJust draft) abbr = Text.pack (show tableNumber)
				| otherwise = squareAbbr abbr
	render (TeXComm "nontermdef" [FixArg (TeXRaw s)]) = render anchor
		{ aId    = "nt:" ++ s
		, aText  = s ++ ":"
		, aClass = "nontermdef" }
	render (TeXComm "weblink" [FixArg text, FixArg href])
		= render anchor
			{ aText = simpleRender text
			, aHref = simpleRender href}
	render (TeXComm "url" [FixArg url])
		= render anchor
			{ aText = simpleRender url
			, aHref = simpleRender url }
	render (TeXComm "link" [FixArg txt, FixArg (rmClause -> TeXComm "ref" [FixArg abbr])])
		= \ctx -> render anchor{aHref=abbrHref abbr ctx, aText = render txt ctx{inLink=True}} ctx
	render (TeXComm comm
				[ FixArg txt
				, FixArg (parseIndex -> (p, _))
				, FixArg (rmClause -> TeXComm "ref" [FixArg abbr])])
		| comm `elem` words "linkx deflinkx liblinkx"
		= \ctx -> render anchor
			{ aText = render txt ctx{inLink=True}
			, aHref = Text.pack (show SectionToSection) ++ "/" ++ url abbr
				++ "#" ++ cat comm ++ indexPathHref p
			} ctx
		where
			cat "linkx" = ""
			cat "deflinkx" = "def"
			cat "liblinkx" = "lib"
	render (TeXComm "grammarterm_" [FixArg (TeXRaw section), FixArg (TeXRaw name)]) =
		\sec -> xml "i" [] $ if inLink sec
			then name
			else render anchor{aHref=grammarNameRef section name, aText=name} sec
	render (TeXComm "texttt" [FixArg x]) = \ctx -> spanTag "texttt" $
		render x ctx{rawHyphens = True, insertBreaks = True}
	render (TeXComm "tcode" [FixArg x]) = \ctx ->
		spanTag (if inCodeBlock ctx then "tcode_in_codeblock" else "texttt") $
			render x ctx{rawHyphens = True, insertBreaks = True}
	render (TeXComm "textbf" [FixArg x]) = ("<b>" ++) . (++ "</b>") . render x
	render (TeXComm "index"
			[ FixArg (TeXRaw (Text.unpack -> read -> entryNr))
			, OptArg (TeXRaw category)
			, FixArg (parseIndex -> (p, kind))
			])
		= case kind of
			Just IndexClose -> const ""
			Just (See _ _) -> const ""
			_ -> \ctx ->
				let
					idSuffix :: Text
					idSuffix = indexOccurrenceSuffix ctx entryNr
				in
					spanTag "indexparent" $ render anchor
						{ aId = indexPathId category p ++ idSuffix
						, aClass = "index"} ctx
	render (TeXComm "defnx"
		[ FixArg (TeXRaw (Text.unpack -> read -> entryNr))
		, FixArg txt
		, FixArg (parseIndex -> (p, _)) ])
		= \ctx -> let suffix = indexOccurrenceSuffix ctx entryNr in
			render anchor
				{ aText  = xml "i" [] $ render txt ctx{inLink=True}
				, aId    = "def" ++ indexPathId "generalindex" p ++ suffix
				, aHref  = "#def" ++ indexPathHref p ++ suffix
				, aClass = "hidden_link" } ctx
	render (TeXComm "indexedspan" [FixArg text, FixArg indices])
		= \ctx -> foldl f (render text ctx) indexPaths
		where
			f t p = xml "span" [("id", uncurry indexPathId p)] t
			indexPaths :: [(Text {- category -}, IndexPath)]
			indexPaths =
				[ (cat, p)
				| [FixArg _num, OptArg (TeXRaw cat), FixArg (parseIndex -> (p, _))] <- lookForCommand "index" indices]
	render (TeXEnv "indexed" [FixArg indices] content)
		= \ctx -> foldl f (render content ctx) indexPaths
		where
			f t p = xml "div" [("id", uncurry indexPathId p)] t
			indexPaths :: [(Text, IndexPath)]
			indexPaths =
				[ (cat, p) | [FixArg _num, OptArg (TeXRaw cat), FixArg (parseIndex -> (p, _))] <- lookForCommand "index" indices]
	render (TeXComm "discretionary" _) = const zwsp
	render (TeXComm "multicolumn" [FixArg (TeXRaw n), _, FixArg content]) = xml "td" [("colspan", n)] . render content
	render (TeXComm "leftshift" [FixArg content]) =
		(spanTag "mathsf" "lshift" ++) . xml "sub" [("class", "math")] . render content
	render (TeXComm "verb" [FixArg a]) = \c -> xml "code" [] $ render a c{rawTilde=True, rawHyphens=True}
	render (TeXComm "footnoteref" [FixArg (TeXRaw n)]) = \ctx -> flip render ctx $ anchor
		{ aClass = "footnotenum"
		, aText  = n
		, aHref  =
			(if isJust (page ctx) then "" else "SectionToSection/" ++ paraUrl ctx)
			++ "#footnote-" ++ n }
	render (TeXComm "raisebox" args)
		| FixArg (TeXRaw d) <- head args
		, FixArg content <- Prelude.last args =
			let neg s
				| Text.head s == '-' = Text.tail s
				| otherwise = "-" ++ s
			in xml "span" [("style", "position: relative; top: " ++ neg d)] . render content
	render (TeXComm "parbox" [_, FixArg x]) = render x
	render (TeXComm "term" [FixArg x]) =
		\sec ->
			let
				i = "def:" ++ asId x
				-- It's tempting to use 'term:' instead of 'def:' here, but if we do that,
				-- URLs break when upstream promotes a \term to a \defn.
			in render anchor
				{ aText  = "<i>" ++ render x sec ++ "</i>"
				, aId    = i
				, aHref  = "#" ++ urlChars i
				, aClass = "hidden_link" } sec
	render (TeXComm "texorpdfstring" [_, FixArg x]) = render x
	render t@(TeXComm (dropTrailingWs -> x) s)
	    | x `elem` kill                = return ""
	    | null s, Just y <-
	       lookup x simpleMacros       = return y
	    | [FixArg z] <- s, Just y <-
	       lookup x simpleMacros       = (y ++) . render z
	    | x `elem` makeSpan            = spanTag (Text.pack x) . render (map texFromArg s)
	    | otherwise                    = error $ "render: unexpected: " ++ show t
	render (TeXCommS " ")              = return "&nbsp;"
	render (TeXCommS "\n")             = return "\n"
	render (TeXCommS (dropTrailingWs -> s))
	    | s `elem` literal             = return $ Text.pack s
	    | Just x <-
	       lookup s simpleMacros       = return x
	    | s `elem` kill                = return ""
	    | otherwise                    = return $ spanTag (Text.pack s) ""
	render (TeXEnv "itemdecl" [] t)    = \c -> xml "code" [("class", "itemdecl")] $
	                                     Text.dropWhile (== '\n') $
	                                     render t c{rawTilde=True, rawHyphens=True}
	render env@(TeXEnv e _ t)
	    | e `elem` makeSpan            = spanTag (Text.pack e) . render t
	    | e `elem` makeDiv             = xml "div" [("class", Text.pack e)] . render t
	    | isMath env && isComplexMath env = return $ renderComplexMath env
	    | isCodeblock env              = renderCodeblock env
	    | otherwise                    = error $ "render: unexpected env " ++ e

instance Render Int where render = return . Text.pack . show

instance Render IndexComponent where
	render IndexComponent{..} = render indexKey

instance Render IndexEntry where
	render IndexEntry{indexEntryKind=Just (See also x), ..} = \ctx ->
		"<i>" ++ (if also then "see also" else "see") ++ "</i> " ++
			 render (anchor
				 { aHref = "#:" ++
				 (urlChars $
				  replace " " "_" $
				  replace ", " "," $
				  indexKeyContent x)
				 , aText = render x ctx}) ctx
	render IndexEntry{indexEntryKind=Just IndexClose} = return ""
	render IndexEntry{..} =
		return $ simpleRender anchor
			{ aHref = "SectionToSection/" ++ url abbr
				++ "#" ++ extraIdPrefix ++ indexPathHref indexPath
			, aText = squareAbbr abbr }
		where
			extraIdPrefix
				| indexEntryKind == Just DefinitionIndex = "def"
				| indexCategory == "libraryindex" = "lib"
				| otherwise = ""
			abbr = abbreviation indexEntrySection

instance Render IndexTree where
	render y sec = go [] y
		where
			go :: IndexPath -> Map.Map IndexComponent IndexNode -> Text
			go up x = mconcat $ f up . Map.toList x

			f :: IndexPath -> (IndexComponent, IndexNode) -> Text
			f up (comp, IndexNode{..}) =
				let
					up' = up ++ [comp]
				in
					xml "div" [("id", indexPathId "" up')] $
					xml "div" [("class", "indexitems")] $
					Text.intercalate ", " (nub $ filter (/= "") $ render comp sec : flip render sec . indexEntries) ++
					go up' indexSubnodes

renderTab :: Bool -> Table -> RenderContext -> Text
renderTab stripTab Table{..} sec =
	xml "div" [("class", "numberedTable"), ("id", id_)] $ -- todo: multiple abbrs?
		"Table " ++ render anchor{aText = render tableNumber sec, aHref = "#" ++ id_} sec ++ " — " ++
		render tableCaption sec ++ "<br>" ++ renderTable columnSpec tableBody sec
	where
		id_ = (if stripTab then replace "tab:" "" else id) $ render (head tableAbbrs) sec

renderFig :: Bool -> Figure -> Text
renderFig stripFig Figure{..} =
	xml "div" [("class", "figure"), ("id", id_)] $
		figureSvg ++ "<br>" ++
		"Figure " ++ simpleRender anchor{aText=simpleRender figureNumber, aHref="#" ++ id_} ++ " — " ++
		simpleRender figureName
	where id_ = (if stripFig then replace "fig:" "" else id) $ simpleRender figureAbbr

data RenderItem = RenderItem { listOrdered :: Bool, item :: Item }

instance Render RenderItem where
	render RenderItem{item=Item Nothing elems} ctx = xml "li" [] $ render elems ctx
	render RenderItem{item=Item (Just nn) elems, ..} ctx = xml "li" [("id", thisId)] $ addItemLink $ render elems ctx'
		where
			left
				| listOrdered = "-4.5em"
				| otherwise = simpleRender (-3 - 2 * length nn - extraIndentation ctx) ++ "em"
			thisId = idPrefix ctx ++ Text.pack (Prelude.last nn)
			ctx' = ctx{ idPrefix = thisId ++ "." }
			dottedNumber = Text.intercalate "." (Text.pack . nn)
			linkText
				| listOrdered =
					let
						s = Prelude.last nn
						punct
							| isAlpha (head s) = ")"
							| otherwise = "."
					in
						Text.pack $ s ++ punct
				| otherwise = "(" ++ dottedNumber ++ ")"
			linkClass
				| listOrdered = "enumerated_item_num"
				| otherwise = "marginalized"
			addItemLink :: Text -> Text
			addItemLink x
				| listOrdered = "<p>" ++ render link ctx' ++ fromJust (Text.stripPrefix "<p >" x)
					-- The link needs to be in the <p> so that it shares the baseline.
				| otherwise = xml "div" [("class", "marginalizedparent"), ("style", "left:" ++ left)] (render link ctx') ++ x
			link = anchor
					{ aClass = linkClass
					, aHref  = if isJust (page ctx)
						then "#" ++ thisId
						else "SectionToSection/" ++ paraUrl ctx ++ "#" ++ dottedNumber
					, aText  = linkText }

paraUrl :: RenderContext -> Text
paraUrl = url . abbreviation . paraSection . nearestEnclosingPara

instance Render Footnote where
	render (Footnote n content) ctx =
			xml "div" [("class", "footnote"), ("id", "footnote-" ++ num)] $
			xml "div" [("class", "footnoteNumberParent")]
			(render link ctx) ++
			render content ctx
		where
			num = render n ctx
			link = anchor
				{ aText  = num ++ ")"
				, aHref  =
					(if isJust (page ctx) then "" else "SectionToSection/" ++ paraUrl ctx)
					++ "#footnote-" ++ num
				, aClass = "marginalized" }

instance Render Element where
	render (LatexElements [env@(TeXEnv "indexed" _ _)]) = render env
	render (LatexElements [TeXEnv "minipage" [_] (trim -> cb@(TeXEnv "codeblock" [] t))]) =
		xml "div" [("class", "minipage")] . renderCodeblock cb
	render (LatexElements [env]) | isCodeblock env = render env
	render (LatexElements t) = \sec ->
		case Text.stripStart (render (mconcat t) sec) of "" -> ""; x -> xml "p" [] x
	render (Bnf e t)
		| e `elem` makeBnfTable = renderBnfTable (Text.pack e) t
		| e `elem` makeBnfPre = bnfPre (Text.pack e) . render (trimr $ preprocessPre t)
		| otherwise = error "unexpected bnf"
	render (TableElement t) = renderTab False t
	render (Tabbing t) =
		xml "pre" [] . htmlTabs . render (preprocessPre t)
	render (FigureElement f) = return $ renderFig False f
	render Enumerated{..} = xml t [("class", Text.pack enumCmd)] .
			render (RenderItem (enumCmd == "enumerate" || enumCmd == "enumeratea") . enumItems)
		where
			t = case enumCmd of
				"enumeratea" -> "ol"
				"enumerate" -> "ol"
				"itemize" -> "ul"
				"description" -> "ul"
				_ -> undefined
	render (FootnoteElement e) = render e

allText :: LaTeX -> [Text]
allText (TeXRaw x) = [x]
allText (TeXSeq x y) = allText x ++ allText y
allText (TeXComm _ args) = concat (map (allText . texFromArg) args)
allText (TeXEnv _ _ x) = allText x
allText (TeXBraces x) = allText x
allText (TeXMath _ x) = allText x
allText _ = []

trimText :: Text -> Text
trimText = Text.dropWhile isSpace . Text.dropWhileEnd isSpace

isComplexMath :: LaTeX -> Bool
isComplexMath t =
	(not . null $ matchCommand (`elem` complexCmds) t)
	|| (not . null $ matchEnv (`elem` ["array", "eqnarray"]) t)
	|| (Text.any (`elem` ("+-*/^_=, " :: String)) $ trimText $ Text.concat $ allText t)
	where complexCmds = words "frac sum binom int sqrt lfloor rfloor lceil rceil log mathscr"

data RenderContext = RenderContext
	{ page :: Maybe Section
	, draft :: Maybe Draft
	, nearestEnclosingPara :: Paragraph
	, rawHyphens :: Bool -- in real code envs /and/ in \texttt
	, rawTilde :: Bool   -- in real code envs but not in \texttt
	, rawSpace :: Bool
	, insertBreaks :: Bool
	, inLink :: Bool -- so as not to linkify grammarterms that appear as part of a defined/linkified term/phrase
	, inCodeBlock :: Bool -- in codeblocks, some commands like \tcode have a different meaning
	, replXmlChars :: Bool -- replace < with &lt;, etc
	, extraIndentation :: Int -- in em
	, idPrefix :: Text }

defaultRenderContext :: RenderContext
defaultRenderContext = RenderContext
	{ page = Nothing
	, draft = Nothing
	, nearestEnclosingPara = error "no para"
	, rawHyphens = False
	, rawTilde = False
	, rawSpace = False
	, insertBreaks = False
	, inLink = False
	, inCodeBlock = False
	, replXmlChars = True
	, extraIndentation = 0
	, idPrefix = "" }

squareAbbr :: Render a => a -> Text
squareAbbr x = "[" ++ simpleRender x ++ "]"

remoteTableHref :: Table -> Text
remoteTableHref Table{tableSection=Section{..}, ..} =
	"SectionToSection/" ++ url abbreviation ++ "#" ++ url (head tableAbbrs)

remoteFigureHref :: Figure -> Text
remoteFigureHref Figure{figureSection=Section{..}, ..} =
	"SectionToSection/" ++ url abbreviation ++ "#" ++ url figureAbbr

linkToRemoteTable :: Table -> Anchor
linkToRemoteTable t = anchor{ aHref = remoteTableHref t }

linkToRemoteFigure :: Figure -> Anchor
linkToRemoteFigure f = anchor{ aHref = remoteFigureHref f }

abbrHref :: LaTeX -> RenderContext -> Text
abbrHref abbr RenderContext{..}
	| "fig:" `isPrefixOf` simpleRender abbr =
		if abbr `elem` (figureAbbr . figures page) then "#" ++ url abbr
		else remoteFigureHref (figureByAbbr (fromJust draft) abbr)
	| "tab:" `isPrefixOf` simpleRender abbr =
		case tableByAbbr (fromJust draft) abbr of
			Just t | not ([abbr] `elem` (tableAbbrs . snd . tables page)) -> remoteTableHref t
			_ -> "#" ++ url abbr
	| otherwise = linkToSectionHref SectionToSection abbr

renderMath :: LaTeX -> RenderContext -> Text
renderMath m sec
	| isComplexMath m = renderComplexMath m
	| otherwise = spanTag s $ renderSimpleMath m sec
	where
		s = mathKind m
		mathKind (TeXMath Square _) = "mathblock"
		mathKind _ = "math"

renderSimpleMath :: LaTeX -> RenderContext -> Text
renderSimpleMath (TeXRaw s) sec =
	case suffix of
		Just ('^', rest) -> italicise prefix ++ output "sup" rest
		Just ('_', rest) -> italicise prefix ++ output "sub" rest
		_ -> italicise s
	where
		(prefix, suffix') = Text.break (`elem` ['^', '_']) s
		suffix = Text.uncons suffix'

		output tag rest =
			case Text.uncons rest of
				Just (c, rest') -> xml tag [] (italicise $ Text.singleton c) ++ (renderSimpleMath (TeXRaw rest') sec)
				Nothing -> error "Malformed math"

		italicise t =
			case Text.span isAlpha t of
				("", "") -> ""
				("", rest) ->
					case Text.uncons rest of
						Just (c, rest') -> entities c ++ italicise rest'
						Nothing -> error ""
				(alpha, rest) -> spanTag "mathalpha" alpha ++ italicise rest

		entities :: Char -> Text
		entities '<' = "&lt;"
		entities '>' = "&gt;"
		entities c = Text.singleton c
renderSimpleMath (TeXSeq (TeXRaw s) rest) sec
	| last `elem` ["^", "_"] =
		renderSimpleMath (TeXRaw $ Text.reverse $ Text.drop 1 s') sec
		++ xml tag [] (renderSimpleMath content sec)
		++ renderSimpleMath rest' sec
	| otherwise = renderSimpleMath (TeXRaw s) sec ++ renderSimpleMath rest sec
	where
		s' = Text.reverse s
		last = Text.take 1 s'
		tag = case last of
			"^" -> "sup"
			"_" -> "sub"
			_ -> error ""
		(content, rest') = case rest of
			(TeXSeq a b) -> (a, b)
			other -> (other, TeXEmpty)
renderSimpleMath (TeXBraces x) sec = renderSimpleMath x sec
renderSimpleMath (TeXSeq (TeXComm "frac" [(FixArg num)]) rest) sec =
	"[" ++ renderSimpleMath num sec ++ "] / [" ++ renderSimpleMath den sec ++ "]" ++ renderSimpleMath rest' sec
	where
		(den, rest') = findDenum rest
		findDenum (TeXSeq (TeXBraces d) r) = (d, r)
		findDenum (TeXSeq _ r) = findDenum r
		findDenum r = (r, TeXEmpty)
renderSimpleMath (TeXSeq a b) sec = (renderSimpleMath a sec) ++ (renderSimpleMath b sec)
renderSimpleMath (TeXMath Dollar m) sec = renderSimpleMath (trim m) sec
renderSimpleMath (TeXMath _ m) sec = renderSimpleMath m sec
renderSimpleMath other sec = render other sec

renderComplexMath :: LaTeX -> Text
renderComplexMath x = case x of
		TeXMath kind t -> memodRenderMath (prepMath t) (kind == Dollar)
		TeXEnv "eqnarray*" [] _ -> memodRenderMath (prepMath x) False
		_ -> memodRenderMath (prepMath x) True
	where
		prepMath = Text.unpack . TeXRender.render . cleanup
		cleanup :: LaTeX -> LaTeX
		cleanup (TeXSeq x y) = TeXSeq (cleanup x) (cleanup y)
		cleanup (TeXComm "tcode" x) = TeXComm "texttt" (map (mapTeXArg cleanup) x)
		cleanup (TeXComm "ensuremath" [FixArg x]) = cleanup x
		cleanup (TeXComm "discretionary" _) = TeXEmpty
		cleanup (TeXCommS "hfill") = TeXEmpty
		cleanup (TeXCommS "break") = TeXEmpty
		cleanup (TeXCommS "br") = TeXEmpty
		cleanup (TeXCommS "-") = TeXEmpty
		cleanup (TeXComm x y) = TeXComm x (map (mapTeXArg cleanup) y)
		cleanup x@(TeXRaw _) = x
		cleanup x@(TeXCommS _) = x
		cleanup (TeXBraces x) = TeXBraces (cleanup x)
		cleanup x@(TeXComment _) = x
		cleanup (TeXEnv x y z) = TeXEnv x (map (mapTeXArg cleanup) y) (cleanup z)
		cleanup x@TeXEmpty = x
		cleanup (TeXMath x y) = TeXMath x (cleanup y)
		cleanup x@(TeXLineBreak _ _) = x

rmTrailingNewline :: Text -> Text
rmTrailingNewline (Text.stripSuffix "\n" -> Just x) = x
rmTrailingNewline x = x

memodRenderMath :: String -> Bool -> Text
memodRenderMath = memo2 $ \s inline -> unsafePerformIO $ do
	let args = ["--inline" | inline] ++ ["--", s]
	formula <- Text.replace " focusable=\"false\"" "" 
		. rmTrailingNewline -- Prevents artifacts in [rand.adapt.ibits]#4
		. Text.pack
		. kill " id=\"(MJXc|MathJax)-[0-9A-Za-z-]+\""
		. kill " style=\"\""
		. readProcess "/usr/lib/node_modules/mathjax-node/bin/tex2html" args ""
	return $ if inline then formula else "</p><p style='text-align:center'>" ++ formula ++ "</p><p>"
	where
		kill r s = subRegex (mkRegex r) s ""


renderTable :: LaTeX -> [Row [Element]] -> RenderContext -> Text
renderTable colspec a sec =
	xml "table" [] (renderRows (parseColspec $ Text.unpack $ stripColspec colspec) a)
	where
		stripColspec (TeXRaw s) = s
		stripColspec (TeXSeq x y) = stripColspec x ++ stripColspec y
		stripColspec _ = ""

		parseColspec :: String -> [Text]
		parseColspec ('|' : rest) = pc rest
		parseColspec other = pc other

		pc ('|' : []) = []
		pc ('|' : letter : rest) =
			"border " ++ (colClass letter) : pc rest
		pc (letter : rest) =
			colClass letter : pc rest
		pc "" = []

		colClass x | x `elem` ['l', 'm', 'x'] = "left"
		colClass 'p' = "justify"
		colClass 'r' = "right"
		colClass 'c' = "center"
		colClass other = error $ "Unexpected column type " ++ (other : [])

		combine newCs oldCs
			| Just _ <- Text.stripPrefix "border" oldCs,
			  Nothing <- Text.stripPrefix "border" newCs = "border " ++ newCs
			| otherwise = newCs

		renderRows _ [] = ""
		renderRows cs (Row{..} : rest) =
			(xml "tr" cls $ renderCols cs 1 clines cells) ++ renderRows cs rest
			where
				cls | RowSep <- rowSep = [("class", "rowsep")]
				    | CapSep <- rowSep = [("class", "capsep")]
				    | otherwise = []
				clines
					| Clines clns <- rowSep = clns
					| otherwise = []

		renderCols _ _ _ [] = ""
		renderCols (c : cs) colnum clines (Cell{..} : rest)
			| length cs < length rest = undefined
			| Multicolumn w cs' <- cellSpan =
				let
					[c''] = parseColspec $ Text.unpack $ stripColspec cs'
					c' = combine c'' c ++ clineClass colnum clines
					colspan
						| null rest = length cs + 1
						| otherwise = w
				in
					(xml "td" [("colspan", render colspan sec), ("class", c')] $ renderCell content sec)
					++ renderCols (drop (colspan - 1) cs) (colnum + colspan) clines rest
			| otherwise =
				(xml "td" [("class", c ++ clineClass colnum clines)] $ renderCell content sec)
				++ renderCols cs (colnum + 1) clines rest
		renderCols [] _ _ (_ : _) = error "Too many columns"

		clineClass n clines
			| isJust $ find (\(begin, end) -> begin <= n && n <= end) clines =
				" cline"
			| otherwise = ""

renderCell :: [Element] -> RenderContext -> Text
renderCell e sec = mconcat (map renderCell' e)
	where
		renderCell' (LatexElements t) = render (mconcat t) sec
		renderCell' other = render other sec

-- Explicit <br/>'s are redundant in <pre>, so strip them.
preprocessPre :: LaTeX -> LaTeX
preprocessPre (TeXLineBreak _ _) = TeXEmpty
preprocessPre (TeXCommS (dropTrailingWs -> "br")) = TeXEmpty
preprocessPre (TeXEnv e a c) = TeXEnv e a (preprocessPre c)
preprocessPre (TeXSeq a b) = TeXSeq (preprocessPre a) (preprocessPre b)
preprocessPre rest = rest

makeTabs :: Text -> Text
	-- Instead of implementing the internal mechanics of the bnf
	-- environments for real, we just turn leading whitespace into
	-- a tab.
makeTabs = Text.unlines . map f . Text.lines
	where
		f :: Text -> Text
		f x = let (a, b) = Text.span (== ' ') x in
			if Text.length a >= 2 then "&#9;" ++ b else x

bnfPre :: Text -> Text -> Text
bnfPre c = xml "pre" [("class", c)] . makeTabs . Text.strip

htmlTabs :: Text -> Text
htmlTabs = replace "\t" "&#9;" -- todo: still necessary?

renderBnfTable :: Text -> LaTeX -> RenderContext -> Text
renderBnfTable c l = bnfPre c . htmlTabs . render (trimr $ preprocessPre l)

grammarNameRef :: Text -> Text -> Text
grammarNameRef s n = "SectionToSection/" ++ s ++ "#nt:" ++ (Text.toLower n)

data Link = TocToSection | SectionToToc | SectionToSection
	deriving Show

linkToSectionHref :: Link -> LaTeX -> Text
linkToSectionHref link abbr = Text.pack (show link) ++ "/" ++ url abbr

linkToSection :: Link -> LaTeX -> Anchor
linkToSection link abbr = anchor{ aHref = linkToSectionHref link abbr, aText = squareAbbr abbr }

url :: LaTeX -> Text
url = urlChars . flip render defaultRenderContext{replXmlChars = False}

simpleRender :: Render a => a -> Text
simpleRender = flip render defaultRenderContext

secnum :: Text -> Section -> Text
secnum href Section{sectionNumber=n,..} =
	simpleRender (anchor{aClass=c, aHref=href, aText=text, aStyle=Text.pack style})
	where
		style = "min-width:" ++ show (73 + length parents * 15) ++ "pt"
		text
			| chapter == InformativeAnnex, null parents = "Annex " ++ chap ++ "&emsp;(informative)"
			| chapter == NormativeAnnex, null parents = "Annex " ++ chap ++ "&emsp;(normative)"
			| otherwise = Text.intercalate "." (chap : simpleRender . tail ns)
		ns = reverse $ n : sectionNumber . parents
		c	| chapter /= NormalChapter, null parents = "annexnum"
			| otherwise = "secnum"
		chap :: Text
		chap
			| chapter == NormalChapter = simpleRender (head ns)
			| otherwise = Text.pack [['A'..] !! head ns]

abbreviations :: Section -> [LaTeX]
abbreviations Section{..} = abbreviation : concatMap abbreviations subsections

fileContent :: Text -> Text -> Text -> Text -> Text
fileContent pathHome title extraHead body =
	"<!DOCTYPE html>" ++
	"<html lang='en'>" ++
		"<head>" ++
			"<title>" ++ title ++ "</title>" ++
			"<meta charset='UTF-8'/>" ++
			"<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "14882.css'/>" ++
			"<link rel='icon' href='icon.png'/>" ++
			extraHead ++
		"</head>" ++
		"<body><div class='wrapper'>" ++ body ++ "</div></body>" ++
	"</html>"

abbrAsPath :: LaTeX -> Text
abbrAsPath = flip render defaultRenderContext{replXmlChars = False}

data SectionFileStyle = Bare | WithExtension | InSubdir
	deriving (Eq, Read)

doLink :: SectionFileStyle -> Link -> Text -> Text
doLink sfs l = go . Text.splitOn (Text.pack (show l) ++ "/")
	where
		go (x : (Text.break (`elem` ("'#" :: String)) -> (a, b)) : z) = x ++ f a ++ go (b : z)
		go [x] = x
		go _ = undefined
		f :: Text -> Text
		f u = case (sfs, l) of
			(Bare, SectionToToc) -> "./#" ++ u
			(Bare, TocToSection) -> dotSlashForColon u
			(Bare, SectionToSection) -> dotSlashForColon u
			(InSubdir, SectionToToc) -> "../#" ++ u
			(InSubdir, TocToSection) -> u ++ "/"
			(InSubdir, SectionToSection) -> "../" ++ u
			(WithExtension, SectionToToc) -> "index.html#" ++ u
			(WithExtension, TocToSection) -> dotSlashForColon u ++ ".html"
			(WithExtension, SectionToSection) -> dotSlashForColon u ++ ".html"
		dotSlashForColon x = if ':' `elem` Text.unpack x then "./" ++ x else x
			-- Without this, we generate urls like "string::replace.html",
			-- in which "string" is parsed as the protocol.

applySectionFileStyle :: SectionFileStyle -> Text -> Text
applySectionFileStyle sfs =
	doLink sfs SectionToSection
	. doLink sfs SectionToToc
	. doLink sfs TocToSection

outputDir :: FilePath
outputDir = "14882/"
