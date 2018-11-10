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
	Render(render), concatRender, url, renderTab, renderFig, simpleRender, simpleRender2, squareAbbr,
	linkToSection, secnum, SectionFileStyle(..), applySectionFileStyle, Page(..), parentLink,
	fileContent, Link(..), outputDir, linkToRemoteTable, defaultRenderContext, isSectionPage,
	abbrAsPath, abbreviations, RenderContext(..), renderLatexParas, makeMathMap, MathMap, extractMath
	) where

import Load14882 (parseIndex) -- todo: bad
import Document (
	CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Draft(..), Footnote(..),
	TeXPara(..), Sentence(..),
	Section(..), Chapter(..), Table(..), Figure(..), Sections(..), figures, tables, Item(..),
	IndexComponent(..), IndexTree, IndexNode(..), IndexKind(..), IndexEntry(..),
	IndexPath, indexKeyContent, tableByAbbr, figureByAbbr, Paragraph(..), Note(..), Example(..))
import LaTeXBase (LaTeX, LaTeXUnit(..), ArgKind(..), MathType(..), matchCommand, matchEnv, lookForCommand, renderLaTeX, trim, trimr, needsSpace, isMath, isCodeblock, texStripPrefix)
import qualified Data.IntMap as IntMap
import Data.Text (isPrefixOf)
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Char (isAlpha, isSpace)
import Control.Arrow (second)
import qualified Prelude
import Prelude hiding (take, (.), (++), writeFile)
import System.Process (readProcess)
import Text.Regex (mkRegex, subRegex)
import Data.List (find, nub, intersperse)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Util ((.), (++), replace, Text, xml, spanTag, anchor, Anchor(..), greekAlphabet, dropTrailingWs, urlChars, intercalateBuilders)

kill, literal :: [String]
kill = words $
	"clearpage renewcommand newcommand enlargethispage noindent indent vfill pagebreak setlength " ++
	"caption capsep continuedcaption bottomline hline rowsep hspace endlist cline itcorr " ++
	"hfill nocorr small endhead kill footnotesize rmfamily microtypesetup nobreak nolinebreak " ++
	"label topline FlushAndPrintGrammar left right protect = ! @ - xspace"
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
	, ("vdots"          , "&#8942;")
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
	, ("land"           , "∧")
	, ("lor"            , "∨")
	, ("cdot"           , "·")
	, ("cdots"          , "⋯")
	, ("to"             , "→")
	, ("rightarrow"     , "→")
	, ("mapsto"         , "↦")
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
	, ("textmu"         , "μ")
	, ("tablerefname"   , "Table")
	, ("newline"        , "<br>")
	, (">"              , "&#9;")
	, ("bnfindent"      , "&#9;")
	, ("\n"             , "\n")
	]
	++ [(n, Text.pack [c]) | (n, c) <- greekAlphabet]

zwsp :: Text
zwsp = "&#x200b;" -- U+200B ZERO WIDTH SPACE

makeSpan, makeDiv, makeBnf :: [String]
makeSpan = words "center mbox mathsf emph terminal textsc phantom term mathtt textnormal textrm descr textsl textit mathit indented"
makeDiv = words "definition cvqual emph exitnote footnote mathit paras ttfamily TableBase table tabular longtable"
makeBnf = words "bnf ncbnf simplebnf ncsimplebnf"

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
asId = mconcat . map f
	where
		f :: LaTeXUnit -> Text
		f (TeXRaw t) = replace "\n" "_" $ replace " " "_" t
		f (TeXComm "tcode" [(_, x)]) = asId x
		f (TeXComm "texttt" [(_, x)]) = asId x
		f (TeXComm "textit" [(_, x)]) = asId x
		f (TeXComm "mathsf" [(_, x)]) = asId x
		f (TeXComm "xspace" []) = "_"
		f (TeXBraces x) = asId x
		f (TeXMath Dollar x) = asId x
		f (TeXComm "texorpdfstring" [_, (_, x)]) = asId x
		f x = error $ "asId: unexpected: " ++ show x

instance Render Anchor where
	render Anchor{..} _ =
		xml "a" ([("class", aClass) | aClass /= ""] ++
		         [("href" , aHref ) | aHref  /= ""] ++
		         [("id"   , aId   ) | aId    /= ""] ++
		         [("style", aStyle) | aStyle /= ""]) aText

class Render a where render :: a -> RenderContext -> TextBuilder.Builder

concatRender :: Render a => [a] -> RenderContext -> TextBuilder.Builder
concatRender x c = mconcat $ map (\y -> render y c) x

instance Render Char where render c _ = TextBuilder.singleton c

instance (Render a, Render b) => Render (a, b) where
	render (x, y) = render x ++ render y

rmClause :: LaTeX -> LaTeX
rmClause (TeXRaw "Clause~" : x) = x
rmClause (TeXRaw "Clause " : x) = x
rmClause (TeXRaw "Table~" : x) = x
rmClause (TeXRaw "Annex~" : x) = x
rmClause x = x

redundantOpen :: Text -> Bool
redundantOpen (Text.unpack -> (c:'(':s))
	= (c `elem` ("~ \n" :: String))
	&& (s `elem` ["", "Clause ", "Clause~"])
redundantOpen _ = False

renderCodeblock :: LaTeX -> RenderContext -> TextBuilder.Builder
renderCodeblock code ctx = xml "pre" [("class", "codeblock")] $
	render (trimr code) ctx{rawTilde=True, rawHyphens=True, rawSpace=True, inCodeBlock=True}

sameIdNamespace :: Maybe IndexKind -> Maybe IndexKind -> Bool
sameIdNamespace Nothing (Just IndexOpen) = True
sameIdNamespace (Just IndexOpen) Nothing = True
sameIdNamespace x y = x == y

abbrIsOnPage :: LaTeX -> Page -> Bool
abbrIsOnPage abbr page = page == FullPage || abbr `elem` (abbreviation . sections page)
			
indexOccurrenceSuffix :: RenderContext -> Int -> Text
	-- Returns the _ that distinguishes expr#def:object_expression from
	-- expr#def:object_expression_ ([expr] has two definitions of 'object expression',
	-- one for E1.E2 and one for E1.*E2.)
indexOccurrenceSuffix c indexNum = Text.pack $ replicate numPre '_'
	where
		m	| SectionPage s <- page c = secIndexEntries s
			| otherwise = indexEntryMap (draft c)
		(pre, Just theEntry, _post) = IntMap.splitLookup indexNum m
		p e = indexPath e == indexPath theEntry &&
			indexCategory e == indexCategory theEntry &&
			sameIdNamespace (indexEntryKind e) (indexEntryKind theEntry)
		numPre = IntMap.size $ IntMap.filter p pre

instance Render LaTeX where
	render ( gt@(TeXComm "grammarterm_" [(FixArg, [TeXRaw termSec]),  _])
	       : ps@(TeXComm "textit" [(FixArg, [TeXRaw "s"])])
	       : TeXComm "nolinebreak" _
	       : TeXRaw openParen
	       : TeXComm "ref" [(FixArg, [TeXRaw refSec])]
	       : (texStripPrefix ")" -> Just rest))
		| refSec == termSec
		, redundantOpen openParen
		= render (gt : ps : rest)
	render ( gt@(TeXComm "grammarterm_" [(FixArg, [TeXRaw termSec]),  _])
	       : TeXComm "nolinebreak" _
	       : TeXRaw openParen
	       : TeXComm "ref" [(FixArg, [TeXRaw refSec])]
	       : (texStripPrefix ")" -> Just rest))
		| refSec == termSec
		, redundantOpen openParen
		= render (gt : rest)
	render (TeXComm (dropTrailingWs -> "xspace") [] : x) = (if needsSpace x then (" " ++) else id) . render x
	render (TeXComm "textbackslash" [] : y)
		| (TeXRaw s : rest) <- y  = \sec -> "\\" ++ render (TeXRaw $ if rawSpace sec then s else unspace s) sec ++ render rest sec
		where
			unspace s
				| Just (c, cc) <- Text.uncons s, isSpace c = cc
				| otherwise = s
	render (TeXComm "itshape" [] : x) = ("<i>" ++) . (++ "</i>") . render x
	render (x : y) = render x ++ render y
	render [] = return ""

instance Render LaTeXUnit where
	render (TeXRaw x                 ) = \ctx -> TextBuilder.fromText $
	                                     (if rawHyphens ctx then id
	                                         else replace "--" "–" . replace "---" "—")
	                                   $ (if rawTilde ctx then id else replace "~" " ")
	                                   $ (if insertBreaks ctx then
	                                         replace "::" (zwsp ++ "::" ++ zwsp) .
	                                         replace "_" "_&shy;"
	                                      else id)
	                                   $ (if replXmlChars ctx then
	                                         replace ">" "&gt;" .
	                                         replace "<" "&lt;" .
	                                         replace "&" "&amp;"
	                                      else id)
	                                   $ x
	render (TeXComm "br" []          ) = return "<br/>"
	render  TeXLineBreak               = return "<br/>"
	render (TeXComm "break" []       ) = return "<br/>"
	render (TeXBraces t              ) = render t
	render m@(TeXMath _ _            ) = renderMath [m]
	render (TeXComm "comment" [(FixArg, comment)]) = \c ->
	    spanTag "comment" $ render comment c{rawTilde=False, rawHyphens=False}
	render (TeXComm "ensuremath" [(FixArg, x)]) = renderMath x
	render (TeXComm "ref" [(FixArg, abbr)]) = \ctx@RenderContext{..} ->
		let
			linkText :: TextBuilder.Builder
			linkText
				| "tab:" `isPrefixOf` simpleRender abbr
				, Just Table{..} <- tableByAbbr draft abbr = TextBuilder.fromString (show tableNumber)
				| otherwise = squareAbbr abbr
		in
			simpleRender2 anchor{aHref = abbrHref abbr ctx, aText = linkText}
	render (TeXComm "nontermdef" [(FixArg, [TeXRaw s])]) = render anchor
		{ aId    = "nt:" ++ s
		, aText  = TextBuilder.fromText s ++ ":"
		, aHref  = "#nt:" ++ s
		, aClass = "nontermdef" }
	render (TeXComm "weblink" [(FixArg, text), (FixArg, href)])
		= render anchor
			{ aText = simpleRender2 text
			, aHref = simpleRender href}
	render (TeXComm "url" [(FixArg, u)])
		= render anchor
			{ aText = simpleRender2 u
			, aHref = simpleRender u }
	render (TeXComm "link" [(FixArg, txt), (FixArg, (rmClause -> [TeXComm "ref" [(FixArg, abbr)]]))])
		= \ctx -> render anchor{aHref=abbrHref abbr ctx, aText = render txt ctx{inLink=True}} ctx
	render (TeXComm comm
				[ (FixArg, txt)
				, (FixArg, (parseIndex -> (p, _)))
				, (FixArg, (rmClause -> [TeXComm "ref" [(FixArg, abbr)]]))])
		| comm `elem` words "linkx deflinkx liblinkx"
		= \ctx -> render anchor
			{ aText = render txt ctx{inLink=True}
			, aHref = (if abbrIsOnPage abbr (page ctx) then "" else linkToSectionHref SectionToSection abbr)
				++ "#" ++ cat comm ++ indexPathHref p
			} ctx
		where
			cat "linkx" = ""
			cat "deflinkx" = "def"
			cat "liblinkx" = "lib"
			cat _ = undefined
	render (TeXComm "grammarterm_" [(FixArg, [TeXRaw section]), (FixArg, [TeXRaw name])]) =
		\sec -> xml "i" [] $ if inLink sec
			then TextBuilder.fromText name
			else render anchor{aHref=grammarNameRef section name, aText=TextBuilder.fromText name} sec
	render (TeXComm "texttt" [(FixArg, x)]) = \ctx -> spanTag "texttt" $ render x ctx{rawHyphens = True, insertBreaks = True}
	render (TeXComm "tcode" [(FixArg, x)]) = \ctx ->
		spanTag (if inCodeBlock ctx then "tcode_in_codeblock" else "texttt") $
			render x ctx{rawHyphens = True, insertBreaks = True}
	render (TeXComm "textbf" [(FixArg, x)]) = ("<b>" ++) . (++ "</b>") . render x
	render (TeXComm "index"
			[ (FixArg, [TeXRaw (Text.unpack -> read -> entryNr)])
			, (OptArg, [TeXRaw category])
			, (FixArg, (parseIndex -> (p, kind)))
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
		[ (FixArg, [TeXRaw (Text.unpack -> read -> entryNr)])
		, (FixArg, txt)
		, (FixArg, (parseIndex -> (p, _))) ])
		= \ctx -> let suffix = indexOccurrenceSuffix ctx entryNr in
			render anchor
				{ aText  = xml "i" [] $ render txt ctx{inLink=True}
				, aId    = "def" ++ indexPathId "generalindex" p ++ suffix
				, aHref  = "#def" ++ indexPathHref p ++ suffix
				, aClass = "hidden_link" } ctx
	render (TeXComm "indexedspan" [(FixArg, text), (FixArg, indices)])
		= \ctx -> foldl f (render text ctx) indexPaths
		where
			f t p = xml "span" [("id", uncurry indexPathId p)] t
			indexPaths :: [(Text {- category -}, IndexPath)]
			indexPaths =
				[ (cat, p)
				| [(FixArg, _num), (OptArg, [TeXRaw cat]), (FixArg, (parseIndex -> (p, _)))] <- lookForCommand "index" indices]
	render (TeXEnv "indexed" [(FixArg, indices)] content)
		= \ctx -> foldl f (render content ctx) indexPaths
		where
			f t p = xml "div" [("id", uncurry indexPathId p)] t
			indexPaths :: [(Text, IndexPath)]
			indexPaths =
				[ (cat, p) | [(FixArg, _num), (OptArg, [TeXRaw cat]), (FixArg, (parseIndex -> (p, _)))] <- lookForCommand "index" indices]
	render (TeXComm "discretionary" _) = const (TextBuilder.fromText zwsp)
	render (TeXComm "ifthenelse" [_, _, (FixArg, x)]) = render x
	render (TeXComm "multicolumn" [(FixArg, [TeXRaw n]), _, (FixArg, content)]) = xml "td" [("colspan", n)] . render content
	render (TeXComm "leftshift" [(FixArg, content)]) =
		(spanTag "mathsf" "lshift" ++) . xml "sub" [("class", "math")] . render content
	render (TeXComm "verb" [(FixArg, a)]) = \c -> xml "code" [] $ render a c{rawTilde=True, rawHyphens=True}
	render (TeXComm "footnoteref" [(FixArg, [TeXRaw n])]) = \ctx -> flip render ctx $ anchor
		{ aClass = "footnotenum"
		, aText  = TextBuilder.fromText n
		, aHref  =
			(if page ctx == FullPage || isSectionPage (page ctx) then "" else "SectionToSection/" ++ paraUrl ctx)
			++ "#footnote-" ++ n }
	render (TeXComm "raisebox" args)
		| (FixArg, [TeXRaw d]) <- head args
		, (FixArg, content) <- Prelude.last args =
			let neg s
				| Text.head s == '-' = Text.tail s
				| otherwise = "-" ++ s
			in xml "span" [("style", "position: relative; top: " ++ neg d)] . render content
	render (TeXComm "parbox" [_, (FixArg, x)]) = render x
	render (TeXComm "term" [(FixArg, x)]) =
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
	render (TeXComm "texorpdfstring" [_, (FixArg, x)]) = render x
	render (TeXComm " " [])            = return "&nbsp;"
	render (TeXComm "\n" [])           = return "\n"
	render (TeXComm (dropTrailingWs -> s) [])
	    | s `elem` literal             = return $ TextBuilder.fromString s
	    | Just x <-
	       lookup s simpleMacros       = return $ TextBuilder.fromText x
	    | s `elem` kill                = return ""
	    | otherwise                    = return $ spanTag (Text.pack s) ""
	render (TeXComm (dropTrailingWs -> x) s)
	    | x `elem` kill                = return ""
	    | null s, Just y <-
	       lookup x simpleMacros       = return $ TextBuilder.fromText y
	    | [(FixArg, z)] <- s, Just y <-
	       lookup x simpleMacros       = (TextBuilder.fromText y ++) . render z
	    | otherwise                    = spanTag (Text.pack x) . render (s >>= snd)
	render (TeXEnv "itemdecl" [(FixArg, [TeXRaw num])] t) = \c ->
		let
			i = idPrefix c ++ "itemdecl:" ++ num
			link = anchor{aClass="itemDeclLink", aHref="#" ++ urlChars i, aText="🔗"}
		in
			xml "div" [("class", "itemdecl"), ("id", i)] $
			xml "div" [("class", "marginalizedparent")] (render link c) ++
			xml "code" [("class", "itemdeclcode")] (TextBuilder.fromText $ Text.dropWhile (== '\n') $ LazyText.toStrict $ TextBuilder.toLazyText $ render t c{rawTilde=True, rawHyphens=True})
	render env@(TeXEnv e _ t)
	    | e `elem` makeSpan            = spanTag (Text.pack e) . render t
	    | e `elem` makeDiv             = xml "div" [("class", Text.pack e)] . render t
	    | isMath env && isComplexMath [env] = TextBuilder.fromText . renderComplexMath [env]
	    | isCodeblock env              = renderCodeblock t
		| e == "minipage", [TeXEnv "codeblock" [] cb] <- trim t =
			xml "div" [("class", "minipage")] . renderCodeblock cb
	    | otherwise                    = error $ "render: unexpected env " ++ e

instance Render Int where render = return . TextBuilder.fromString . show

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
		return $ simpleRender2 anchor
			{ aHref = "SectionToSection/" ++ url abbr
				++ "#" ++ extraIdPrefix ++ indexPathHref indexPath
			, aText = squareAbbr abbr }
		where
			extraIdPrefix
				| isDefinitionIndexEntry && indexEntryKind == Nothing = "def"
				| indexCategory == "libraryindex" = "lib"
				| otherwise = ""
			abbr = abbreviation indexEntrySection

instance Render IndexTree where
	render y ctx = go [] y
		where
			go :: IndexPath -> Map.Map IndexComponent IndexNode -> TextBuilder.Builder
			go up x = mconcat $ f up . Map.toList x

			f :: IndexPath -> (IndexComponent, IndexNode) -> TextBuilder.Builder
			f up (comp, IndexNode{..}) =
				let
					up' = up ++ [comp]
				in
					xml "div" [("id", indexPathId "" up')] $
					xml "div" [("class", "indexitems")] $
					TextBuilder.fromText (
					Text.intercalate ", " (nub $ filter (/= "") $ map (LazyText.toStrict . TextBuilder.toLazyText) $ render comp ctx : flip render ctx . indexEntries)) ++
					go up' indexSubnodes

renderTab :: Bool -> Table -> RenderContext -> TextBuilder.Builder
renderTab stripTab Table{..} sec =
	xml "div" [("class", "numberedTable"), ("id", id_)] $ -- todo: multiple abbrs?
		"Table " ++ render anchor{aText = render tableNumber sec, aHref = "#" ++ id_} sec ++ " — " ++
		render tableCaption sec ++ "<br>" ++ renderTable columnSpec tableBody sec
	where
		id_ = (if stripTab then replace "tab:" "" else id) $ LazyText.toStrict $ TextBuilder.toLazyText $ render (head tableAbbrs) sec

renderFig :: Bool -> Figure -> TextBuilder.Builder
renderFig stripFig Figure{..} =
	xml "div" [("class", "figure"), ("id", id_)] $
		TextBuilder.fromText figureSvg ++ "<br>" ++
		"Figure " ++ simpleRender2 anchor{aText=simpleRender2 figureNumber, aHref="#" ++ id_} ++ " — " ++
		simpleRender2 figureName
	where id_ = (if stripFig then replace "fig:" "" else id) $ simpleRender figureAbbr

data RenderItem = RenderItem { listOrdered :: Bool, item :: Item }

instance Render RenderItem where
	render RenderItem{item=Item Nothing _ elems} ctx = xml "li" [] $ renderLatexParas elems ctx
	render RenderItem{item=Item (Just nn) mlabel elems, ..} ctx =
			xml "li" [("id", thisId)] $ case mlabel of
				Nothing -> itemLink ++ renderLatexParas elems ctx'
				Just label ->
					render anchor{aHref = linkHref, aText=simpleRender2 label} ctx'
					++ " " ++ renderLatexParas elems ctx'
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
			itemLink :: TextBuilder.Builder
			itemLink
				| listOrdered = render link ctx'
				| otherwise = xml "div" [("class", "marginalizedparent"), ("style", "left:" ++ left)] (render link ctx')
			linkHref = "#" ++ thisId
			link = anchor{aClass=linkClass, aHref=linkHref, aText=TextBuilder.fromText linkText}

paraUrl :: RenderContext -> Text
paraUrl RenderContext{..} = url $ abbreviation $ case nearestEnclosing of
	Left p -> paraSection p
	Right s -> s

instance Render Footnote where
	render (Footnote n content) ctx =
			xml "div" [("class", "footnote"), ("id", i)] $
			xml "div" [("class", "footnoteNumberParent")] (render link ctx) ++
			renderLatexParas content ctx{idPrefix = i ++ "."}
		where
			num = Text.pack $ show n
			i = "footnote-" ++ num
			link = anchor
				{ aText  = TextBuilder.fromText $ num ++ ")"
				, aHref  =
					(if page ctx == FullPage || isSectionPage (page ctx) then "" else "SectionToSection/" ++ paraUrl ctx)
					++ "#" ++ i
				, aClass = "marginalized" }

noWrapSpace :: TextBuilder.Builder
noWrapSpace = "<span style='white-space:nowrap'> </span>"

instance Render Note where
	render Note{..} ctx =
			xml "div" [("id", i), ("class", "note")]
				("[" ++ noWrapSpace ++ render link ctx
				++ xml "div" [("class", "noteBody")] (
					"<span class='textit'>:</span> "
					++ renderLatexParas noteContent ctx
					++ " —" ++ noWrapSpace ++ "<i>end note</i>")
				++ noWrapSpace ++ "]")
			++ " "
		where
			i = idPrefix ctx ++ "note-" ++ Text.pack (show noteNumber)
			link = anchor{
				aHref = "#" ++ i,
				aClass = "note_link",
				aText = "<span class='textit'>Note</span>" }

instance Render Example where
	render Example{..} ctx =
			xml "div" [("id", i), ("class", "example")]
				("[" ++ noWrapSpace ++ render link ctx
				++ xml "div" [("class", "exampleBody")] (
					"<span class='textit'>:</span> "
					++ renderLatexParas exampleContent ctx
					++ " —" ++ noWrapSpace ++ "<i>end example</i>")
				++ noWrapSpace ++ "]")
			++ " "
		where
			i = idPrefix ctx ++ "example-" ++ Text.pack (show exampleNumber)
			link = anchor{
				aHref = "#" ++ i,
				aClass = "example_link",
				aText = "<span class='textit'>Example</span>" }

instance Render Element where
	render (LatexElement x) = render x
	render (Codeblock x) = render x
	render (NoteElement x) = render x
	render (ExampleElement x) = render x
	render (Bnf e t)
		| e `elem` makeBnf = bnf (Text.pack e) . LazyText.toStrict . TextBuilder.toLazyText . render (trimr $ preprocessPre t)
		| otherwise = error "unexpected bnf"
	render (TableElement t) = renderTab False t
	render (Tabbing t) =
		xml "pre" [] . TextBuilder.fromText . htmlTabs . LazyText.toStrict . TextBuilder.toLazyText . render (preprocessPre t) -- todo: this is horrible
	render (FigureElement f) = return $ renderFig False f
	render Enumerated{..} = xml t [("class", Text.pack enumCmd)] .
			concatRender (RenderItem (enumCmd == "enumerate" || enumCmd == "enumeratea") . enumItems)
		where
			t = case enumCmd of
				"enumeratea" -> "ol"
				"enumerate" -> "ol"
				"itemize" -> "ul"
				"description" -> "ul"
				_ -> undefined

allText :: LaTeXUnit -> [Text]
allText (TeXRaw x) = [x]
allText (TeXComm _ args) = concatMap (concatMap allText . snd) args
allText (TeXEnv _ _ x) = x >>= allText
allText (TeXBraces x) = x >>= allText
allText (TeXMath _ x) = x >>= allText
allText _ = []

trimText :: Text -> Text
trimText = Text.dropWhile isSpace . Text.dropWhileEnd isSpace

isComplexMath :: LaTeX -> Bool
isComplexMath t =
	(not . null $ matchCommand (`elem` complexCmds) t)
	|| (not . null $ matchEnv (`elem` ["array", "eqnarray"]) t)
	|| (Text.any (`elem` ("+-*/^_=, " :: String)) $ trimText $ Text.concat $ t >>= allText)
	where complexCmds = words "frac sum binom int sqrt lfloor rfloor lceil rceil log mathscr"

data Page = SectionPage Section | FullPage | IndexPage | XrefDeltaPage | FootnotesPage | TablesPage | TocPage
    deriving Eq

isSectionPage :: Page -> Bool
isSectionPage (SectionPage _) = True
isSectionPage _ = False

instance Sections Page where
    sections (SectionPage sec) = sections sec
    sections _ = []

data RenderContext = RenderContext
	{ page :: Page
	, draft :: Draft
	, nearestEnclosing :: Either Paragraph Section
	, rawHyphens :: Bool -- in real code envs /and/ in \texttt
	, rawTilde :: Bool   -- in real code envs but not in \texttt
	, rawSpace :: Bool
	, insertBreaks :: Bool
	, inLink :: Bool -- so as not to linkify grammarterms that appear as part of a defined/linkified term/phrase
	, inCodeBlock :: Bool -- in codeblocks, some commands like \tcode have a different meaning
	, replXmlChars :: Bool -- replace < with &lt;, etc
	, extraIndentation :: Int -- in em
	, idPrefix :: Text
	, mathMap :: MathMap }

defaultRenderContext :: RenderContext
defaultRenderContext = RenderContext
	{ page = error "no page"
	, draft = error "no draft"
	, nearestEnclosing = error "no para/sec"
	, rawHyphens = False
	, rawTilde = False
	, rawSpace = False
	, insertBreaks = False
	, inLink = False
	, inCodeBlock = False
	, replXmlChars = True
	, extraIndentation = 0
	, idPrefix = ""
	, mathMap = Map.empty }

squareAbbr :: Render a => a -> TextBuilder.Builder
squareAbbr x = "[" ++ simpleRender2 x ++ "]"

remoteTableHref :: Table -> Text
remoteTableHref Table{tableSection=Section{..}, ..} =
	"SectionToSection/" ++ url abbreviation ++ "#" ++ url (head tableAbbrs)

remoteFigureHref :: Figure -> Text
remoteFigureHref Figure{figureSection=Section{..}, ..} =
	"SectionToSection/" ++ url abbreviation ++ "#" ++ url figureAbbr

linkToRemoteTable :: Table -> Anchor
linkToRemoteTable t = anchor{ aHref = remoteTableHref t }

--linkToRemoteFigure :: Figure -> Anchor
--linkToRemoteFigure f = anchor{ aHref = remoteFigureHref f }

parentLink :: Section -> LaTeX -> Text
parentLink parent child
	| Just sub <- Text.stripPrefix (LazyText.toStrict $ TextBuilder.toLazyText $ render (abbreviation parent) ctx ++ ".") secname = sub
	| otherwise = secname
	where
		secname = LazyText.toStrict $ TextBuilder.toLazyText $ render child ctx
		ctx = defaultRenderContext{replXmlChars=False}

abbrHref :: LaTeX -> RenderContext -> Text
abbrHref abbr RenderContext{..}
	| "fig:" `isPrefixOf` simpleRender abbr =
		if page == FullPage || abbr `elem` (figureAbbr . figures page) then "#" ++ url abbr
		else remoteFigureHref (figureByAbbr draft abbr)
	| "tab:" `isPrefixOf` simpleRender abbr =
		case tableByAbbr draft abbr of
			Just t | page /= FullPage, not ([abbr] `elem` (tableAbbrs . snd . tables page)) -> remoteTableHref t
			_ -> "#" ++ url abbr
	| abbrIsOnPage abbr page = "#" ++ case page of
	    SectionPage sec -> urlChars (parentLink sec abbr)
	    _ -> url abbr
	| otherwise = linkToSectionHref SectionToSection abbr

extractMath :: LaTeX -> Maybe (String, Bool)
extractMath [TeXMath Dollar (TeXComm "text" [(FixArg, _)] : more)] = extractMath [TeXMath Dollar more]
extractMath [TeXMath Dollar (TeXComm "tcode" _ : more)] = extractMath [TeXMath Dollar more]
extractMath m | not (isComplexMath m) = Nothing
extractMath m = if isComplexMath m then Just (mathKey m) else Nothing

prepMath :: LaTeX -> String
prepMath = Text.unpack . renderLaTeX . (>>= cleanup)
  where
    cleanup :: LaTeXUnit -> LaTeX
    cleanup (TeXComm "tcode" x) = [TeXComm "texttt" (map (second (>>= cleanup)) x)]
    cleanup (TeXComm "ensuremath" [(FixArg, x)]) = x >>= cleanup
    cleanup (TeXComm "discretionary" _) = []
    cleanup (TeXComm "hfill" []) = []
    cleanup (TeXComm "break" []) = []
    cleanup (TeXComm "br" []) = []
    cleanup (TeXComm "-" []) = []
    cleanup (TeXComm "quad " []) = [TeXRaw " "] -- because MathJax does not support \quad
    cleanup (TeXComm x y) = [TeXComm x (map (second (>>= cleanup)) y)]
    cleanup x@(TeXRaw _) = [x]
    cleanup (TeXBraces x) = [TeXBraces (x >>= cleanup)]
    cleanup (TeXEnv x y z) = [TeXEnv x (map (second (>>= cleanup)) y) (z >>= cleanup)]
    cleanup (TeXMath x y) = [TeXMath x (y >>= cleanup)]
    cleanup x@TeXLineBreak = [x]

renderMath :: LaTeX -> RenderContext -> TextBuilder.Builder
renderMath [TeXMath Dollar (TeXComm "text" [(FixArg, stuff)] : more)] ctx =
  render stuff ctx ++ renderMath [TeXMath Dollar more] ctx
renderMath [TeXMath Dollar (c@(TeXComm "tcode" _) : more)] ctx =
  render c ctx ++ renderMath [TeXMath Dollar more] ctx
renderMath m ctx
	| isComplexMath m = TextBuilder.fromText $ renderComplexMath m ctx
	| otherwise = spanTag s $ renderSimpleMath m ctx
	where
		s = mathKind m
		mathKind [TeXMath Square _] = "mathblock"
		mathKind _ = "math"

renderSimpleMath :: LaTeX -> RenderContext -> TextBuilder.Builder
renderSimpleMath [] _ = ""
renderSimpleMath (TeXRaw s : rest) sec
	| tlast `elem` ["^", "_"] =
		renderSimpleMathUnit (TeXRaw $ Text.reverse $ Text.drop 1 s') sec
		++ xml tag [] (renderSimpleMath content sec)
		++ renderSimpleMath rest' sec
	| otherwise = renderSimpleMathUnit (TeXRaw s) sec ++ renderSimpleMath rest sec
	where
		s' = Text.reverse s
		tlast = Text.take 1 s'
		tag = case tlast of
			"^" -> "sup"
			"_" -> "sub"
			_ -> error ""
		(content, rest') = case rest of
			(a : b) -> ([a], b)
			other -> (other, [])
renderSimpleMath (TeXComm "frac" [(FixArg, num)] : rest) sec =
	"[" ++ renderSimpleMath num sec ++ "] / [" ++ renderSimpleMath den sec ++ "]" ++ renderSimpleMath rest' sec
	where
		(den, rest') = findDenum rest
		findDenum (TeXBraces d : r) = (d, r)
		findDenum (_ : r) = findDenum r
		findDenum r = (r, [])
renderSimpleMath (x : y) ctx = renderSimpleMathUnit x ctx ++ renderSimpleMath y ctx

renderSimpleMathUnit :: LaTeXUnit -> RenderContext -> TextBuilder.Builder
renderSimpleMathUnit (TeXRaw s) sec =
	case suffix of
		Just ('^', rest) -> italicise prefix ++ output "sup" rest
		Just ('_', rest) -> italicise prefix ++ output "sub" rest
		_ -> italicise s
	where
		(prefix, suffix') = Text.break (`elem` ['^', '_']) s
		suffix = Text.uncons suffix'

		output :: Text -> Text -> TextBuilder.Builder
		output tag rest =
			case Text.uncons rest of
				Just (c, rest') -> xml tag [] (italicise $ Text.singleton c) ++ (renderSimpleMathUnit (TeXRaw rest') sec)
				Nothing -> error "Malformed math"

		italicise :: Text -> TextBuilder.Builder
		italicise t =
			case Text.span isAlpha t of
				("", "") -> TextBuilder.fromString ""
				("", rest) ->
					case Text.uncons rest of
						Just (c, rest') -> entities c ++ italicise rest'
						Nothing -> error ""
				(alpha, rest) -> spanTag "mathalpha" (TextBuilder.fromText alpha) ++ italicise rest

		entities :: Char -> TextBuilder.Builder
		entities '<' = "&lt;"
		entities '>' = "&gt;"
		entities c = TextBuilder.singleton c
renderSimpleMathUnit (TeXBraces x) sec = renderSimpleMath x sec
renderSimpleMathUnit (TeXMath Dollar m) sec = renderSimpleMath (trim m) sec
renderSimpleMathUnit (TeXMath _ m) sec = renderSimpleMath m sec
renderSimpleMathUnit other sec = render other sec

mathKey :: LaTeX -> (String, Bool)
mathKey m = case m of
		[TeXMath kind t] -> (prepMath t, kind == Dollar)
		[TeXEnv "eqnarray*" [] _] -> (prepMath m, False)
		_ -> (prepMath m, True)

renderComplexMath :: LaTeX -> RenderContext -> Text
renderComplexMath math ctx
    | inline = html
    | otherwise = "</p><p style='text-align:center'>" ++ html ++ "</p><p>"
    where
        k@(_, inline) = mathKey math
        html = fromJust $ Map.lookup k $ mathMap ctx

rmTrailingNewline :: Text -> Text
rmTrailingNewline (Text.stripSuffix "\n" -> Just x) = x
rmTrailingNewline x = x

batchedRenderMath :: [(String, Bool)] -> IO [Text]
batchedRenderMath formulas = do
        out <- readProcess "./mathjax-batch" [] stdin
        return
            $ splitResults "" 
            $ Text.lines
            $ Text.replace " focusable=\"false\"" ""
            $ rmTrailingNewline -- Prevents artifacts in [rand.adapt.ibits]#4
            $ Text.pack
            $ rm " id=\"(MJXc|MathJax)-[0-9A-Za-z-]+\""
            $ rm " style=\"\""
            $ out
    where
        rm r s = subRegex (mkRegex r) s ""
        stdinLines :: [String]
        stdinLines = concatMap (\(f, inline) -> [f] ++ [if inline then "INLINE" else "NONINLINE"]) formulas
        stdin :: String
        stdin = unlines stdinLines

        splitResults :: Text -> [Text] -> [Text]
        splitResults _ [] = []
        splitResults x (a:b)
            | a == "DONE" = x : splitResults "" b
            | otherwise = splitResults (if x == "" then a else x ++ "\n" ++ a) b

type MathMap = Map.Map (String, Bool) Text

makeMathMap :: [(String, Bool)] -> IO MathMap
makeMathMap formulas = Map.fromList . zip formulas . batchedRenderMath formulas

renderTable :: LaTeX -> [Row [TeXPara]] -> RenderContext -> TextBuilder.Builder
renderTable colspec a sec =
	xml "table" [] (renderRows (parseColspec $ Text.unpack $ stripColspec colspec) a)
	where
		stripColspec :: LaTeX -> Text
		stripColspec = mconcat . (>>= f)
			where
				f (TeXRaw s) = [s]
				f _ = []

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
					(xml "td" [("colspan", Text.pack $ show colspan), ("class", c')] $ renderLatexParas content sec)
					++ renderCols (drop (colspan - 1) cs) (colnum + colspan) clines rest
			| otherwise =
				(xml "td" [("class", c ++ clineClass colnum clines)] $ renderLatexParas content sec)
				++ renderCols cs (colnum + 1) clines rest
		renderCols [] _ _ (_ : _) = error "Too many columns"

		clineClass n clines
			| isJust $ find (\(begin, end) -> begin <= n && n <= end) clines =
				" cline"
			| otherwise = ""

instance Render TeXPara where
	render = (mconcat .) . (intersperse " " .) . mapM render . sentences

instance Render Sentence where
	render Sentence{..} ctx =
			case i of
				Nothing -> r $ map Left $ sentenceElems
				Just v ->
					xml "div" [("id", v), ("class", "sentence")] $
						r $ reverse $ linkifyFullStop $ reverse sentenceElems
		where
			r :: [Either Element TextBuilder.Builder] -> TextBuilder.Builder
			r [] = ""
			r (span (\x -> case x of Left (LatexElement _) -> True; _ -> False) -> (ee@(_:_), rest))
				= render ((\(Left (LatexElement x)) -> x) . ee) ctx
				++ r rest
			r (Right x : xs) = x ++ r xs
			r (Left x : xs) = render x ctx ++ r xs

			i = case sentenceNumber of
				Just v -> Just $ idPrefix ctx ++ "sentence-" ++ Text.pack (show v)
				Nothing -> Nothing
			link = anchor{aText = ".", aHref = "#" ++ fromJust i, aClass = "hidden_link"}
			linkifyFullStop :: [Element] -> [Either Element TextBuilder.Builder]
			linkifyFullStop [] = []
			linkifyFullStop (LatexElement (TeXRaw (Text.stripSuffix "." -> Just s)) : xs)
				= Right (simpleRender2 link) : Left (LatexElement $ TeXRaw s) : (Left . xs)
			linkifyFullStop (LatexElement (TeXRaw (Text.stripSuffix ".)" -> Just s)) : xs)
				= Right (simpleRender2 link ++ ")") : Left (LatexElement $ TeXRaw s) : (Left . xs)
			linkifyFullStop (fn@(LatexElement (TeXComm "footnoteref" _)) : xs)
				= Left fn : linkifyFullStop xs
			linkifyFullStop xs = Left . xs

renderLatexParas :: [TeXPara] -> RenderContext -> TextBuilder.Builder
renderLatexParas [] _ = ""
renderLatexParas (TeXPara [] : y) c = renderLatexParas y c
renderLatexParas [x] ctx = render x ctx
renderLatexParas (p@(TeXPara x) : xs@(y : _)) ctx
	| LatexElement _ <- last (sentenceElems (last x)), needsBreak y
		= render p ctx
			++ "<div style='height:0.6em;display:block'></div>"
			++ renderLatexParas xs ctx
	| otherwise = render p ctx ++ renderLatexParas xs ctx

needsBreak :: TeXPara -> Bool
needsBreak (TeXPara []) = False
needsBreak (TeXPara (Sentence _ [] : y)) = needsBreak (TeXPara y)
needsBreak (TeXPara (Sentence s (x : y) : z))
	| noise x = needsBreak (TeXPara (Sentence s y : z))
	| LatexElement _ <- x = True
	| NoteElement _ <- x = True
	| ExampleElement _ <- x = True
	| otherwise = False
	where
		noise (LatexElement (TeXComm "index" _)) = True
		noise (LatexElement (TeXRaw t)) = all isSpace (Text.unpack t)
		noise _ = False


-- Explicit <br/>'s are redundant in <pre>, so strip them.
preprocessPre :: LaTeX -> LaTeX
preprocessPre = concatMap f
	where
		f TeXLineBreak = []
		f (TeXComm (dropTrailingWs -> "br") []) = []
		f (TeXEnv e a c) = [TeXEnv e a (preprocessPre c)]
		f x = [x]

makeTabs :: Text -> Text
	-- Instead of implementing the internal mechanics of the bnf
	-- environments for real, we just turn leading whitespace into
	-- a tab.
makeTabs = Text.unlines . map f . Text.lines
	where
		f :: Text -> Text
		f x = let (a, b) = Text.span (== ' ') x in
			if Text.length a >= 2 then "&#9;" ++ b else x

bnf :: Text -> Text -> TextBuilder.Builder
bnf c x = xml "pre" [("class", c)] $ TextBuilder.fromText $ makeTabs $ Text.strip x

htmlTabs :: Text -> Text
htmlTabs = replace "\t" "&#9;" -- todo: still necessary?

grammarNameRef :: Text -> Text -> Text
grammarNameRef s n = "SectionToSection/" ++ s ++ "#nt:" ++ (Text.toLower n)

data Link = TocToSection | SectionToToc | SectionToSection
	deriving Show

linkToSectionHref :: Link -> LaTeX -> Text
linkToSectionHref link abbr = Text.pack (show link) ++ "/" ++ url abbr

linkToSection :: Link -> LaTeX -> Anchor
linkToSection link abbr = anchor{ aHref = linkToSectionHref link abbr, aText = squareAbbr abbr }

url :: LaTeX -> Text
url = urlChars . LazyText.toStrict . TextBuilder.toLazyText . flip render defaultRenderContext{replXmlChars = False}

simpleRender :: Render a => a -> Text
simpleRender = LazyText.toStrict . TextBuilder.toLazyText . simpleRender2

simpleRender2 :: Render a => a -> TextBuilder.Builder
simpleRender2 = flip render defaultRenderContext

secnum :: Text -> Section -> TextBuilder.Builder
secnum href Section{sectionNumber=n,..} =
	simpleRender2 (anchor{aClass=c, aHref=href, aText=text, aStyle=Text.pack style})
	where
		style = "min-width:" ++ show (73 + length parents * 15) ++ "pt"
		text
			| chapter == InformativeAnnex, null parents = "Annex " ++ chap ++ "&emsp;(informative)"
			| chapter == NormativeAnnex, null parents = "Annex " ++ chap ++ "&emsp;(normative)"
			| otherwise = intercalateBuilders "." (chap : simpleRender2 . tail ns)
		ns = reverse $ n : sectionNumber . parents
		c	| chapter /= NormalChapter, null parents = "annexnum"
			| otherwise = "secnum"
		chap :: TextBuilder.Builder
		chap
			| chapter == NormalChapter = simpleRender2 (head ns)
			| otherwise = TextBuilder.singleton $ ['A'..] !! head ns

abbreviations :: Section -> [LaTeX]
abbreviations Section{..} = abbreviation : concatMap abbreviations subsections

fileContent :: TextBuilder.Builder -> TextBuilder.Builder -> TextBuilder.Builder -> TextBuilder.Builder -> TextBuilder.Builder
fileContent pathHome title extraHead body =
	"<!DOCTYPE html>" ++
	"<html lang='en'>" ++
		"<head>" ++
			"<title>" ++ title ++ "</title>" ++
			"<meta charset='UTF-8'/>" ++
			"<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "14882.css'/>" ++
			"<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "expanded.css' title='Notes and examples expanded'/>" ++
			"<link rel='alternate stylesheet' type='text/css' href='" ++ pathHome ++ "colored.css' title='Notes and examples colored'/>" ++
			"<link rel='alternate stylesheet' type='text/css' href='" ++ pathHome ++ "collapsed.css' title='Notes and examples collapsed'/>" ++
			"<link rel='icon' href='icon.png'/>" ++
			extraHead ++
		"</head>" ++
		"<body><div class='wrapper'>" ++ body ++ "</div></body>" ++
	"</html>"

abbrAsPath :: LaTeX -> Text
abbrAsPath = LazyText.toStrict . TextBuilder.toLazyText . flip render defaultRenderContext{replXmlChars = False}

data SectionFileStyle = Bare | WithExtension | InSubdir
	deriving (Eq, Read)

doLink :: SectionFileStyle -> Link -> Text -> Text
doLink sfs l = LazyText.toStrict . TextBuilder.toLazyText . go . Text.splitOn (Text.pack (show l) ++ "/")
	where
		go :: [Text] -> TextBuilder.Builder
		go (x : (Text.break (`elem` ("'#" :: String)) -> (a, b)) : z) = TextBuilder.fromText x ++ f a ++ go (b : z)
		go [x] = TextBuilder.fromText x
		go _ = undefined
		f :: Text -> TextBuilder.Builder
		f = case (sfs, l) of
			(Bare, SectionToToc) -> ("./#" ++) . TextBuilder.fromText
			(Bare, TocToSection) -> dotSlashForColon
			(Bare, SectionToSection) -> dotSlashForColon
			(InSubdir, SectionToToc) -> ("../#" ++) . TextBuilder.fromText
			(InSubdir, TocToSection) -> (++ "/") . TextBuilder.fromText
			(InSubdir, SectionToSection) -> ("../" ++) . TextBuilder.fromText
			(WithExtension, SectionToToc) -> ("index.html#" ++) . TextBuilder.fromText
			(WithExtension, TocToSection) -> (++ ".html") . dotSlashForColon
			(WithExtension, SectionToSection) -> (++ ".html") . dotSlashForColon
		dotSlashForColon x = (if Text.any (== ':') x then ("./" ++) else id) (TextBuilder.fromText x)
			-- Without dotSlashForColon, we generate urls like "string::replace.html",
			-- in which "string" is parsed as the protocol.

applySectionFileStyle :: SectionFileStyle -> Text -> Text
applySectionFileStyle sfs =
	doLink sfs SectionToSection
	. doLink sfs SectionToToc
	. doLink sfs TocToSection

outputDir :: FilePath
outputDir = "14882/"
