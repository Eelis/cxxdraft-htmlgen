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
	Render(render), concatRender, renderTab, renderFig, renderIndex, simpleRender, simpleRender2, squareAbbr,
	linkToSection, secnum, SectionFileStyle(..), applySectionFileStyle, Page(..), parentLink,
	fileContent, Link(..), outputDir, linkToRemoteTable, defaultRenderContext, isSectionPage,
	RenderContext(..), renderLatexParas
	) where

import Load14882 (parseIndex) -- todo: bad
import Document (
	CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Draft(..), Footnote(..),
	TeXPara(..), Sentence(..), Abbreviation,
	Section(..), Chapter(..), Table(..), Figure(..), Sections(..), figures, tables, Item(..),
	IndexComponent(..), IndexTree, IndexNode(..), IndexKind(..), IndexEntry(..), indexHeading,
	IndexPath, indexKeyContent, tableByAbbr, figureByAbbr, Paragraph(..), Note(..), Example(..))
import LaTeXBase (LaTeX, LaTeXUnit(..), ArgKind(..), MathType(..), matchCommand, matchEnv, lookForCommand, concatRaws,
    renderLaTeX, trim, trimr, isMath, isCodeblock, texStripPrefix, texStripAnyPrefix, texStripInfix, texSpan, unconsRaw, mapTeX)
import qualified Data.IntMap as IntMap
import Data.Text (isPrefixOf)
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Text.HTML.TagSoup as Soup
import Data.Char (isAlpha, isSpace, isAlphaNum, isDigit)
import Control.Arrow (first, second)
import qualified Prelude
import qualified MathJax
import Prelude hiding (take, (.), (++), writeFile)
import Data.List (find, nub, intersperse, (\\))
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Sentences (linkifyFullStop)
import Util ((.), (++), replace, Text, xml, spanTag, anchor, Anchor(..), greekAlphabet, dropTrailingWs,
    urlChars, intercalateBuilders, replaceXmlChars, spanJust, h, partitionBy)

kill, literal :: [String]
kill = words $
	"clearpage renewcommand newcommand enlargethispage noindent indent vfill pagebreak setlength " ++
	"caption capsep continuedcaption bottomline hline rowsep hspace endlist cline itcorr " ++
	"hfill nocorr small endhead kill footnotesize rmfamily microtypesetup nobreak nolinebreak " ++
	"label topline FlushAndPrintGrammar left right protect = ! @ - xspace obeyspaces"
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
	, ("figurerefname"  , "Figure")
	, ("newline"        , "<br>")
	, (">"              , "&#9;")
	, ("bnfindent"      , "&#9;")
	, ("\n"             , "\n")
	]
	++ [(n, Text.pack [c]) | (n, c) <- greekAlphabet]

zwsp :: Text
zwsp = "&#x200b;" -- U+200B ZERO WIDTH SPACE

makeSpan, makeDiv :: [String]
makeSpan = words "center mbox mathsf emph textsc phantom term mathtt textnormal textrm descr textsl textit mathit indented"
makeDiv = words "definition cvqual emph exitnote footnote mathit paras ttfamily TableBase table tabular longtable"

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
	replace "&" "&amp;" .
	indexPathString

indexPathId2 :: RenderContext -> Int -> Text -> IndexPath -> Text
indexPathId2 ctx entryNr p x = indexPathId p x ++ indexOccurrenceSuffix ctx entryNr

indexPathHref :: IndexPath -> Text
indexPathHref = (":" ++) . urlChars . replace "&" "&amp;" . indexPathString

asId :: LaTeX -> Text
asId = mconcat . map f
	where
		f :: LaTeXUnit -> Text
		f (TeXRaw t) = replace "\n" "_" $ replace " " "_" t
		f (TeXComm "tcode" [(_, x)]) = asId x
		f (TeXComm "noncxxtcode" [(_, x)]) = asId x
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

redundantOpen :: Text -> Bool
redundantOpen (Text.unpack -> (c:'(':s))
	= (c `elem` ("~ \n" :: String))
	&& (s `elem` ["", "Clause ", "Clause~"])
redundantOpen _ = False

renderCodeblock :: String -> [(ArgKind, LaTeX)] -> LaTeX -> RenderContext -> TextBuilder.Builder
renderCodeblock env args code ctx =
    (case (env, args) of
      ("codeblocktu", [(FixArg, title)]) -> "<p>" ++ render title ctx ++ ":"
      _ -> "") ++
    xml "pre" [("class", "codeblock")] (
        highlightLines ctx{rawTilde=True, rawHyphens=True, rawSpace=True, inCodeBlock=True} $
        concatRaws $ expandTcode code)
  where
    expandTcode :: LaTeX -> LaTeX
    expandTcode [] = []
    expandTcode (TeXComm "tcode" [(FixArg, x)] : y) = expandTcode (x ++ y)
    expandTcode (x : y) = x : expandTcode y

renderOutputblock :: LaTeX -> RenderContext -> TextBuilder.Builder
renderOutputblock code ctx = xml "pre" [("class", "outputblock")] $
    render code ctx{rawTilde=True, rawHyphens=True, rawSpace=True}

sameIdNamespace :: Maybe IndexKind -> Maybe IndexKind -> Bool
sameIdNamespace Nothing (Just IndexOpen) = True
sameIdNamespace (Just IndexOpen) Nothing = True
sameIdNamespace x y = x == y

abbrIsOnPage :: Abbreviation -> Page -> Bool
abbrIsOnPage abbr page = page == FullPage || abbr `elem` (abbreviation . sections page)

pageIndexEntries :: RenderContext -> IntMap.IntMap IndexEntry
pageIndexEntries c
    | SectionPage s <- page c = secIndexEntries s
    | otherwise = indexEntryMap (draft c)

indexOccurrenceSuffix :: RenderContext -> Int -> Text
	-- Returns the _ that distinguishes expr#def:object_expression from
	-- expr#def:object_expression_ ([expr] has two definitions of 'object expression',
	-- one for E1.E2 and one for E1.*E2.)
indexOccurrenceSuffix c indexNum = Text.pack $ replicate numPre '_'
	where
		(pre, Just theEntry, _post) = IntMap.splitLookup indexNum (pageIndexEntries c)
		p :: IndexEntry -> Bool
		p e = indexPath e == indexPath theEntry &&
			indexCategory e == indexCategory theEntry &&
			isDefinitionIndexEntry e == isDefinitionIndexEntry theEntry &&
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
	render (TeXComm "textbackslash" [] : y)
		| (TeXRaw s : rest) <- y  = \sec -> "\\" ++ render (TeXRaw $ if rawSpace sec then s else unspace s) sec ++ render rest sec
		where
			unspace s
				| Just (c, cc) <- Text.uncons s, isSpace c = cc
				| otherwise = s
	render (TeXComm "itshape" [] : x) = ("<i>" ++) . (++ "</i>") . render x
	render (x : y) = render x ++ render y
	render [] = return ""

keywords :: [Text]
keywords = map Text.pack $ words $
    "char8_t char16_t char32_t namespace struct void operator friend template typedef long short class double public extern " ++
    "using char new union unsigned sizeof alignas typename virtual this return const_cast delete noexcept static_cast " ++
    "reinterpret_cast mutable bool private protected inline constexpr consteval final volatile default explicit enum export asm " ++
    "typeid dynamic_cast throw if else for do while goto auto concept requires decltype try catch static_assert wchar_t " ++
    "case switch alignof break continue signed audit axiom override const register thread_local int float static module import " ++
    "co_return co_await co_yield constinit"
    -- todo: read the real keyword table instead

cppDirectives :: [Text]
cppDirectives = ["include", "define", "else", "elif", "endif", "ifdef", "ifndef", "pragma", "error", "undef", "line", "if"]

texStripHash :: LaTeX -> Maybe LaTeX
texStripHash x
    | Just x' <- texStripPrefix "#" x = Just x'
    | TeXComm "#" [] : x' <- x = Just x'
    | otherwise = Nothing

parseStringLiteral :: LaTeX -> Maybe (LaTeX, LaTeX {- rest -})
parseStringLiteral x
    -- raw:
    | Just (pre, x') <- texStripAnyPrefix ["R\"", "u8R\"", "uR\"", "UR\"", "LR\""] x
    , Just (delim, x'') <- texStripInfix "(" x'
    , Just (body, x''') <- texStripInfix (")" ++ simpleRender delim ++ "\"") (concatRaws $ f x'')
    , (suffix, x'''') <- texSpan (\c -> isAlphaNum c || c == '_') x'''
        = Just ([TeXRaw pre] ++ delim ++ [TeXRaw "("] ++ body ++ [TeXRaw ")"] ++ delim ++ [TeXRaw $ "\"" ++ suffix], x'''')
    -- normal:
    | Just (pre, x') <- texStripAnyPrefix ["\"", "u\"", "U\"", "L\"", "u8\""] x
    , Just (body, x'') <- parseBody x'
    , (suffix, x''') <- texSpan (\c -> isAlphaNum c || c == '_') x''
        = Just ([TeXRaw pre] ++ body ++ [TeXRaw $ "\"" ++ suffix], x''')
    | otherwise = Nothing
    where
        f :: LaTeX -> LaTeX
        f [] = []
        f (TeXComm "~" [] : more) = TeXRaw "~" : f more
        f (TeXBraces [] : more) = f more
        f (hd : t) = hd : f t
        parseBody :: LaTeX -> Maybe (LaTeX, LaTeX {- rest -})
        parseBody [] = Nothing
        parseBody (TeXComm (dropTrailingWs -> "textbackslash") [] : more) = parseBody $ concatRaws $ TeXRaw "\\" : more
        parseBody (TeXRaw (Text.unpack -> raw) : more)
            | '\\':'"':t <- raw = first (TeXRaw "\\\"" :) . parseBody (TeXRaw (Text.pack t) : more)
            | "\"" <- raw = Just ([], more)
            | '"':t <- raw = Just ([], TeXRaw (Text.pack t) : more)
            | raw == "" = parseBody more
            | hd:t <- raw = first (TeXRaw (Text.pack [hd]) :) . parseBody (TeXRaw (Text.pack t) : more)
        parseBody (TeXComm "%" [] : more) = first (TeXComm "%" [] :) . parseBody more
        parseBody (y : more) = first (y :) . parseBody more

parseCharLiteral :: LaTeX -> Maybe (LaTeX, LaTeX {- rest -})
parseCharLiteral x
    | Just (pre, x') <- texStripAnyPrefix ["'", "u'", "L'", "U'", "u8'"] x
    , Just (before, x'') <- texStripInfix "'" x'
    , (suffix, x''') <- texSpan (\c -> isAlphaNum c || c == '_') x''
        = Just ([TeXRaw pre] ++ before ++ [TeXRaw $ "'" ++ suffix], x''')
    | otherwise = Nothing

parseCppDirective :: LaTeX -> Maybe (LaTeX, LaTeX {- rest -})
parseCppDirective x
    | Just x'' <- texStripHash x
    , (spaces, x''') <- texSpan isSpace x''
    , Just (directive, x'''') <- texStripAnyPrefix cppDirectives x'''
        = Just ([TeXRaw ("#" ++ spaces ++ directive)], x'''')
    | otherwise = Nothing

parseSingleLineComment :: LaTeX -> Maybe (LaTeX {- comment -}, LaTeX {- subsequent lines -})
parseSingleLineComment x
    | Just x' <- texStripPrefix "//" x = Just $ case texStripInfix "\n" x' of
        Just (commentLine, moreLines) -> (TeXRaw "//" : commentLine, TeXRaw "\n" : moreLines)
        Nothing -> (x, [])
    | rlap@(TeXComm "rlap" [(FixArg, [TeXComm "textnormal" [(FixArg,[TeXComm "textit" [(FixArg,[TeXRaw "//"])]])]])]) : more <- x
    , Just (commentLine, moreLines) <- texStripInfix "\n" more
        = Just ([rlap, TeXComm "tcode" [(FixArg, commentLine)]], TeXRaw "\n" : moreLines)
    | TeXComm "comment" [(FixArg, c)] : x' <- x = Just (c, x')
    | otherwise = Nothing

parseMany :: (Text -> Maybe (Text, Text)) -> Text -> (Text, Text)
parseMany p t = case p t of
    Nothing -> ("", t)
    Just (x, t') -> first (x++) (parseMany p t')

parseChar :: (Char -> Bool) -> Text -> Maybe (Text, Text)
parseChar p t
    | t /= "", p (Text.head t) = Just (Text.take 1 t, Text.drop 1 t)
    | otherwise = Nothing

parseFirstOf :: [Text -> Maybe (a, Text)] -> Text -> Maybe (a, Text)
parseFirstOf [] _ = Nothing
parseFirstOf (p:pp) t
    | Just r <- p t = Just r
    | otherwise = parseFirstOf pp t

parseSeq :: (Text -> Maybe (Text, Text)) -> (Text -> Maybe (Text, Text)) -> Text -> Maybe (Text, Text)
parseSeq p q t
    | Just (x, t') <- p t
    , Just (y, t'') <- q t' = Just (x ++ y, t'')
    | otherwise = Nothing

parseNumber :: LaTeX -> Maybe (Text, LaTeX)
parseNumber x
    | (raw, more) <- unconsRaw x
    , Just (n, rest) <- (parseStart `parseSeq` (\t -> Just (parseMany parseSuffix t))) raw
        = Just (n, TeXRaw rest : more)
    | otherwise = Nothing
    where
        parseDigit = parseChar isDigit
        parseNonDigit = parseChar (\c -> isAlpha c || c == '_')
        parseStart :: Text -> Maybe (Text, Text)
        parseStart = parseFirstOf [parseChar (== '.') `parseSeq` parseDigit, parseDigit]
        parseSign :: Text -> Maybe (Text, Text)
        parseSign = parseChar (\c -> c == '-' || c == '+')
        parseSuffix :: Text -> Maybe (Text, Text)
        parseSuffix = parseFirstOf
            [ parseDigit
            , parseChar (== '\'') `parseSeq` parseDigit
            , parseChar (== '\'') `parseSeq` parseNonDigit
            , parseChar (`elem` ("eEpP"::String)) `parseSeq` parseSign
            , parseChar (== '.')
            , parseNonDigit
            ]

parseLiteral :: LaTeX -> Maybe (LaTeX, LaTeX)
parseLiteral x
    | Just (number, x') <- parseNumber x = Just ([TeXRaw number], x')
    | Just (lit, x') <- parseCharLiteral x = Just (lit, x')
    | Just (lit, x') <- parseStringLiteral x = Just (lit, x')
    | otherwise = Nothing

parseComment :: LaTeX -> Maybe (LaTeX, LaTeX)
parseComment x
    | Just x' <- texStripPrefix "/*" x, Just (comment, x'') <- texStripInfix "*/" x'
        = Just ([TeXRaw "/*"] ++ comment ++ [TeXRaw "*/"], x'')
    | Just x' <- texStripPrefix "/*" x
        = Just ([TeXRaw "/*"], x')
    | Just x' <- texStripPrefix "*/" x
        = Just ([TeXRaw "*/"], x')
    | Just (comment, x') <- parseSingleLineComment x
        = Just (comment, x')
    | otherwise = Nothing

highlightLines :: RenderContext -> LaTeX -> TextBuilder.Builder
highlightLines ctx x
    | (spaces, x') <- texSpan (== ' ') x, spaces /= "" = TextBuilder.fromText spaces ++ highlightLines ctx x'
    | Just (directive, x') <- parseCppDirective x = spanTag "preprocessordirective" (render directive ctx) ++ highlight ctx x'
    | TeXComm (Text.pack -> c) [(FixArg, y)] : more <- x, c `elem` ["terminal"] = spanTag c (highlightLines ctx y) ++ highlight ctx more
    | i@(TeXComm cmd _) : more <- x, cmd `elem` ["index", "obeyspaces"] = render i ctx ++ highlightLines ctx more
    | otherwise = highlight ctx x

highlightUnit :: RenderContext -> LaTeXUnit -> TextBuilder.Builder
highlightUnit ctx x = case x of
    TeXComm "rlap" [(FixArg, text)] ->
        spanTag "rlap" (highlight ctx text)
    TeXEnv "indexed" [(FixArg, indices)] body ->
        renderIndexed ctx "div" indices (highlight ctx body)
    TeXComm "indexedspan" [(FixArg, text), (FixArg, indices)] ->
        renderIndexed ctx "span" indices (highlight ctx text)
    TeXComm "terminal" [(FixArg, y)] ->
        spanTag "terminal" (highlight ctx y)
    TeXComm c []
        | c `elem` ["%", "&", "caret", "~"] -> spanTag "operator" (render x ctx)
        | c == "#" -> spanTag "preprocessordirective" (render x ctx)
        | c `elem` ["{", "}"] -> spanTag "curlybracket" (render x ctx)
    _ -> render x ctx

highlight :: RenderContext -> LaTeX -> TextBuilder.Builder
highlight ctx x
    | Just x' <- texStripPrefix "\n" x = "\n" ++ highlightLines ctx x'
    | (TeXRaw "" : t) <- x = highlight ctx t
    | Just (lit, x') <- parseLiteral x = spanTag "literal" (render lit ctx) ++ highlight ctx x'
    | Just (comment, x') <- parseComment x = spanTag "comment" (render comment ctx{inComment=True, rawTilde=False}) ++ highlightLines ctx x'
    -- keywords
    | (a, x') <- texSpan p x, a /= "" = (case () of
        _ | a `elem` keywords -> spanTag "keyword"
        _ | a `elem` ["defined", "__has_include", "__has_cpp_attribute", "_Pragma"] -> spanTag "preprocessordirective"
        _ | a `elem` ["nullptr", "true", "false"] -> spanTag "literal"
        _ | otherwise -> id) (render (TeXRaw a) ctx) ++ highlight ctx x'
    where p c = isAlphaNum c || c == '_'
highlight ctx (TeXRaw x : more)
    | Text.head x `elem` ("'\"" :: String) = render (TeXRaw $ Text.take 1 x) ctx ++ highlight ctx (TeXRaw (Text.tail x) : more)
    | Text.head x `elem` ("()"::String) = spanTag "parenthesis" (render (TeXRaw $ Text.take 1 x) ctx) ++ highlight ctx (TeXRaw (Text.tail x) : more)
    | Text.head x `elem` ("{}"::String) = spanTag "curlybracket" (render (TeXRaw $ Text.take 1 x) ctx) ++ highlight ctx (TeXRaw (Text.tail x) : more)
    | Text.head x `elem` ("[]"::String) = spanTag "squarebracket" (render (TeXRaw $ Text.take 1 x) ctx) ++ highlight ctx (TeXRaw (Text.tail x) : more)
    | Text.head x `elem` ("<>"::String) = spanTag "anglebracket" (render (TeXRaw $ Text.take 1 x) ctx) ++ highlight ctx (TeXRaw (Text.tail x) : more)
    | Text.head x == '#' = spanTag "preprocessordirective" "#" ++ highlight ctx (TeXRaw (Text.tail x) : more)
    | Text.take 2 x == "::"
        = spanTag "operator" (render (TeXRaw "::") ctx) ++ highlight ctx (TeXRaw (Text.drop 2 x) : more)
    | Text.head x `elem` ("*&^.-+/!=|:?%~#"::String)
        = spanTag "operator" (render (TeXRaw (Text.take 1 x)) ctx) ++ highlight ctx (TeXRaw (Text.tail x) : more)
    | (a, x') <- Text.span (\c -> not (isAlphaNum c || c `elem` ("#%_(){}[]<>.*:?'\"+=-/|&!^~\n" :: String))) x, a /= ""
        = render (TeXRaw a) ctx ++ highlight ctx (TeXRaw x' : more)
    | otherwise = error ("shit: " ++ show x)
highlight ctx (x : more) = highlightUnit ctx x ++ highlight ctx more
highlight _ [] = ""

renderIndexed :: RenderContext -> Text -> LaTeX -> TextBuilder.Builder -> TextBuilder.Builder
renderIndexed ctx thing indices body = foldl f body indexPaths
	where
	    f t (p, x, entryNr) = xml thing [("id", indexPathId2 ctx entryNr p x)] t
	    indexPaths = [ (cat, p, entryNr)
	                 | [ (FixArg, [TeXRaw (Text.unpack -> read -> entryNr)])
	                   , (OptArg, [TeXRaw cat])
	                   , (FixArg, (parseIndex -> (p, _))) ] <- lookForCommand "index" indices]

commasAnd :: [TextBuilder.Builder] -> TextBuilder.Builder
commasAnd [] = undefined
commasAnd [x] = x
commasAnd [x, y] = x ++ " and " ++ y
commasAnd [x, y, z] = x ++ ", " ++ y ++ ", and " ++ z
commasAnd (x : y) = x ++ ", " ++ commasAnd y

instance Render LaTeXUnit where
	render (TeXRaw x                 ) = \RenderContext{..} -> TextBuilder.fromText
	    $ (if rawHyphens then id else replace "--" "–" . replace "---" "—")
	    $ (if rawTilde then id else replace "~" " ")
	    $ (if insertBreaks then replace "::" (zwsp ++ "::" ++ zwsp) . replace "_" "_&shy;" else id)
	    $ (if replXmlChars then replaceXmlChars else id)
	    $ x
	render (TeXComm "br" []          ) = return "<br/>"
	render  TeXLineBreak               = return "<br/>"
	render (TeXComm "break" []       ) = return "<br/>"
	render (TeXBraces t              ) = render t
	render m@(TeXMath _ _            ) = renderMath [m]
	render (TeXComm "commentellip" []) = const $ spanTag "comment" "/* ... */"
	render (TeXComm "ensuremath" [(FixArg, x)]) = renderMath x
	render (TeXComm "fref" [(FixArg, [TeXRaw abbr])]) = \ctx@RenderContext{..} ->
		let
			linkText :: TextBuilder.Builder
			linkText = TextBuilder.fromString $ ("Figure " ++) $ show $ figureNumber $ figureByAbbr draft ("fig:" ++ abbr)
		in
			simpleRender2 anchor{aHref = abbrHref ("fig:" ++ abbr) ctx, aText = linkText}
	render (TeXComm "ref" [(FixArg, [TeXRaw abbr])]) = \ctx@RenderContext{..} ->
		let
			linkText :: TextBuilder.Builder
			linkText
				| "tab:" `isPrefixOf` abbr
				, Just Table{..} <- tableByAbbr draft abbr = TextBuilder.fromString (show tableNumber)
				| otherwise = squareAbbr abbr
		in
			simpleRender2 anchor{aHref = abbrHref abbr ctx, aText = linkText}
	render (TeXComm "nopnumdiffref" [(FixArg, [TeXRaw (Text.splitOn "," -> abbrs)])]) = \ctx ->
	    let f abbr = simpleRender2 anchor{aHref = abbrHref abbr ctx, aText = squareAbbr abbr}
	    in "<b>Affected " ++ (if length abbrs == 1 then "subclause" else "subclauses") ++ ":</b> "
	        ++ commasAnd (map f abbrs)
	render (TeXComm "nontermdef" [(FixArg, [TeXRaw s])]) =
		(++ spanTag "ntdefcolon" ":") .
		render anchor
		{ aId    = "nt:" ++ s
		, aText  = TextBuilder.fromText s
		, aHref  = "#nt:" ++ s
		, aClass = "nontermdef" }
	render (TeXComm "renontermdef" x) = render (TeXComm "nontermdef" x)
	render (TeXComm "weblink" [(FixArg, text), (FixArg, href)])
		= render anchor
			{ aText = simpleRender2 text
			, aHref = simpleRender href}
	render (TeXComm "url" [(FixArg, u)])
		= render anchor
			{ aText = simpleRender2 u
			, aHref = simpleRender u }
	render (TeXComm "link" [(FixArg, txt), (FixArg, [TeXRaw abbr])])
		= \ctx -> render anchor{aHref=abbrHref abbr ctx, aText = render txt ctx{inLink=True}} ctx
	render (TeXComm comm
				[ (FixArg, txt)
				, (FixArg, (parseIndex -> (p, _)))
				, (FixArg, [TeXRaw abbr])])
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
		\sec -> if inLink sec
			then spanTag "grammarterm" $ TextBuilder.fromText name
			else render anchor{
			    aClass = "grammarterm",
			    aHref = grammarNameRef section name sec,
			    aText = TextBuilder.fromText name} sec
	render (TeXComm "color" _) = const ""
	render (TeXComm "textcolor" [_, (FixArg, x)]) = render x
	render (TeXComm "terminal" [(FixArg, x)]) = spanTag "terminal" . flip highlightLines x
	render (TeXComm "texttt" [(FixArg, x)]) = \ctx -> spanTag "texttt" $ render x ctx{rawHyphens = True, insertBreaks = True}
	render (TeXComm "tcode" [(FixArg, x)]) = \ctx ->
		spanTag (if inCodeBlock ctx then "tcode_in_codeblock" else "texttt") $
		    if not (inComment ctx) && not (inLink ctx) && not (inSectionTitle ctx)
		    then highlightLines ctx{rawHyphens=True, insertBreaks=True} x
		    else render x ctx{rawHyphens=True, insertBreaks=True}
	render (TeXComm "noncxxtcode" [(FixArg, x)]) = \ctx ->
		spanTag (if inCodeBlock ctx then "tcode_in_codeblock" else "texttt") $
		    render x ctx{rawHyphens=True, insertBreaks=True}
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
					if category == "headerindex" then "" else -- needed to prevent duplicate id because \indexhdr also generates a generalindex entry
					spanTag "indexparent" $ render anchor
						{ aId = indexPathId2 ctx entryNr category p
						, aClass = "index"} ctx
	render (TeXComm "defnx"
		[ (FixArg, [TeXRaw (Text.unpack -> read -> entryNr)])
		, (FixArg, txt)
		, (FixArg, (parseIndex -> (p, _))) ])
		= \ctx -> let suffix = indexOccurrenceSuffix ctx entryNr in
			render anchor
				{ aText  = xml "i" [] $ render txt ctx{inLink=True}
				, aId    = "def" ++ indexPathId2 ctx entryNr "generalindex" p
				, aHref  = "#def" ++ indexPathHref p ++ suffix
				, aClass = "hidden_link" } ctx
	render (TeXComm "indexedspan" [(FixArg, text), (FixArg, indices)]) = \ctx -> renderIndexed ctx "span" indices $ render text ctx
	render (TeXEnv "indexed" [(FixArg, indices)] content) = \ctx -> renderIndexed ctx "div" indices $ render content ctx
	render (TeXComm "discretionary" _) = const (TextBuilder.fromText zwsp)
	render (TeXComm "ifthenelse" [_, _, (FixArg, x)]) = render x
	render (TeXComm "multicolumn" [(FixArg, [TeXRaw n]), _, (FixArg, content)]) = xml "td" [("colspan", n)] . render content
	render (TeXComm "leftshift" [(FixArg, content)]) =
		(spanTag "mathsf" "lshift" ++) . xml "sub" [("class", "math")] . render content
	render (TeXComm "verb" [(FixArg, a)]) = \c -> xml "code" [] $ render a c{rawTilde=True, rawHyphens=True}
	render (TeXComm "footnoteref" [(FixArg, [TeXRaw n])]) = \ctx -> flip render ctx $ anchor
		{ aClass = "footnotenum"
		, aText  = TextBuilder.fromText n
		, aId    = "footnoteref-" ++ n
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
	render (TeXComm "textit" [(FixArg, x)]) = \c -> spanTag "textit" $ render x c{rawTilde = False}
	render (TeXComm (dropTrailingWs -> s) [])
	    | s == "caret"                 = return "^"
	    | s `elem` literal             = return $ TextBuilder.fromString s
	    | Just x <-
	       lookup s simpleMacros       = return $ TextBuilder.fromText x
	    | s `elem` kill                = return ""
	    | otherwise                    = return $ spanTag (Text.pack s) ""
	render (TeXComm "class" [(FixArg, [TeXRaw cls]), (FixArg, [TeXComm "href" [(FixArg, [TeXRaw href]), (FixArg, text)]])])
	    = \ctx -> render anchor{aHref=href, aText=render text ctx, aClass=cls} ctx
	render (TeXComm "class" [(FixArg, [TeXRaw cls]), (FixArg, x)])
	    = \ctx -> spanTag cls $ render x ctx
	render (TeXComm "href" [(FixArg, [TeXRaw href]), (FixArg, text)])
	    = \ctx -> render anchor{aHref=href, aText=render text ctx} ctx
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
			xml "code" [("class", "itemdeclcode")] (TextBuilder.fromText $ Text.dropWhile (== '\n') $ LazyText.toStrict $ TextBuilder.toLazyText $ highlightLines c{rawTilde=True, rawHyphens=True} t)
	render env@(TeXEnv e args t)
	    | e `elem` makeSpan            = spanTag (Text.pack e) . render t
	    | e `elem` makeDiv             = xml "div" [("class", Text.pack e)] . render t
	    | isMath env && isComplexMath [env] = renderComplexMath [env]
	    | isCodeblock env              = renderCodeblock e args t
		| e == "minipage", [TeXEnv "codeblock" [] cb] <- trim t =
			xml "div" [("class", "minipage")] . renderCodeblock "codeblock" [] cb
		| e == "outputblock"           = renderOutputblock t
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
			{ aHref = "SectionToSection/" ++ urlChars abbr
				++ "#" ++ extraIdPrefix ++ indexPathHref indexPath
			, aText = (if indexEntryKind == Just BfPage then xml "b" [] else id) $ squareAbbr abbr }
		where
			extraIdPrefix
				| isDefinitionIndexEntry && indexEntryKind == Nothing = "def"
				| indexCategory == "libraryindex" = "lib"
				| otherwise = ""
			abbr = abbreviation indexEntrySection

instance Render [(IndexComponent, IndexNode)] where
	render tree ctx = go [] tree
		where
			go :: IndexPath -> [(IndexComponent, IndexNode)] -> TextBuilder.Builder
			go up x = mconcat $ f up . x

			f :: IndexPath -> (IndexComponent, IndexNode) -> TextBuilder.Builder
			f up (comp, IndexNode{..}) =
				let
					up' = up ++ [comp]
				in
					xml "div" [("id", indexPathId "" up')] $
					xml "div" [("class", "indexitems")] $
					TextBuilder.fromText (
					Text.intercalate ", " (nub $ filter (/= "") $ map (LazyText.toStrict . TextBuilder.toLazyText) $ render comp ctx : flip render ctx . indexEntries)) ++
					go up' (Map.toList indexSubnodes)

renderIndex :: RenderContext -> IndexTree -> String -> TextBuilder.Builder
renderIndex ctx tree "generalindex" = mconcat $ ["<hr>"] ++ linklines ++ ["<hr>"] ++ map sub p
	where
		p = partitionBy (indexHeading . fst) $ Map.toList tree
		sub (n, ii) = h 2 (render anchor{aText=TextBuilder.fromText $ Text.pack n, aId=Text.pack n} ctx) ++ render ii ctx
		(symnum, rest) = splitAt 2 p
		linklines = map (h 2 . mconcat . intersperse " " . map (li . fst)) [symnum, rest]
		li n = render anchor{aText = TextBuilder.fromText $ Text.pack n, aHref = "#" ++ Text.pack n} ctx
renderIndex ctx tree _ = render (Map.toList tree) ctx

renderTab :: Bool -> Table -> RenderContext -> TextBuilder.Builder
renderTab stripTab Table{..} ctx =
	xml "div" [("class", "numberedTable"), ("id", id_)] $ -- todo: multiple abbrs?
		"Table " ++ render anchor{aText = render tableNumber ctx, aHref = "#" ++ id_} ctx ++ ": " ++
		render tableCaption ctx ++ "<br>" ++ renderTable columnSpec tableBody ctx
	where
		id_ = (if stripTab then replace "tab:" "" else id) $ head tableAbbrs

renderFig :: Bool -> Figure -> RenderContext -> TextBuilder.Builder
renderFig stripFig Figure{..} ctx =
	xml "div" [("class", "figure"), ("id", id_)] $
		TextBuilder.fromText figureSvg ++ "<br>" ++
		"Figure " ++ render anchor{aText = render figureNumber ctx, aHref="#" ++ id_} ctx ++ ": " ++
		render figureName ctx ++ "&emsp;&ensp;" ++ squareAbbr figureAbbr
	where id_ = (if stripFig then replace "fig:" "" else id) figureAbbr

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
paraUrl RenderContext{..} = urlChars $ abbreviation $ case nearestEnclosing of
	Left p -> paraSection p
	Right s -> s

instance Render Footnote where
	render (Footnote n content) ctx =
			xml "div" [("class", "footnote"), ("id", i)] $
			xml "div" [("class", "footnoteNumberParent")] (render link ctx) ++
			renderLatexParas content ctx{idPrefix = i ++ "."}
			++ " " ++ render anchor{aText = "⮥", aHref = "#footnoteref-" ++ num} ctx
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
noWrapSpace = "&nbsp;"

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
				aText = spanTag "textit" (TextBuilder.fromText noteLabel) }

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
	render (Bnf e t) = bnf (Text.pack e) . LazyText.toStrict . TextBuilder.toLazyText . render (trimr $ preprocessPre t)
	render (TableElement t) = renderTab False t
	render (Tabbing t) =
		xml "pre" [] . TextBuilder.fromText . htmlTabs . LazyText.toStrict . TextBuilder.toLazyText . render (preprocessPre t) -- todo: this is horrible
	render (FigureElement f) = renderFig False f
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

isComplexMath :: LaTeX -> Bool
isComplexMath t =
	(not . null $ matchCommand (`elem` complexCmds) t)
	|| (not . null $ matchEnv (`elem` ["array", "eqnarray"]) t)
	|| (Text.any (`elem` ("+-*/^_=,' " :: String)) $ Text.strip $ Text.concat $ t >>= allText)
	where complexCmds = words "frac sum binom int sqrt lfloor rfloor lceil rceil log mathscr le"

data Page = SectionPage Section | FullPage | IndexPage | XrefDeltaPage | FootnotesPage | TablesPage | FiguresPage | TocPage
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
	, inComment :: Bool -- in comments, \tcode should not be highlighted
	, inSectionTitle :: Bool -- in section titles, there should be no highlighting
	, inSentence :: Bool -- inside a sentence, we shouldn't generate nested sentence divs
	                     -- (which would happen for sentences inside items inside lists inside sentences)
	, replXmlChars :: Bool -- replace < with &lt;, etc
	, extraIndentation :: Int -- in em
	, idPrefix :: Text }

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
	, inComment = False
	, inSectionTitle = False
	, inSentence = False
	, replXmlChars = True
	, extraIndentation = 0
	, idPrefix = "" }

squareAbbr :: Abbreviation -> TextBuilder.Builder
squareAbbr x = "[" ++ TextBuilder.fromText x ++ "]"

remoteTableHref :: Table -> Text
remoteTableHref Table{tableSection=Section{..}, ..} =
	"SectionToSection/" ++ urlChars abbreviation ++ "#" ++ urlChars (head tableAbbrs)

remoteFigureHref :: Figure -> Text
remoteFigureHref Figure{figureSection=Section{..}, ..} =
	"SectionToSection/" ++ urlChars abbreviation ++ "#" ++ urlChars figureAbbr

linkToRemoteTable :: Table -> Anchor
linkToRemoteTable t = anchor{ aHref = remoteTableHref t }

--linkToRemoteFigure :: Figure -> Anchor
--linkToRemoteFigure f = anchor{ aHref = remoteFigureHref f }

parentLink :: Section -> Abbreviation -> Text
parentLink parent child
	| Just sub <- Text.stripPrefix (abbreviation parent ++ ".") child = sub
	| otherwise = child

abbrHref :: Abbreviation -> RenderContext -> Text
abbrHref abbr RenderContext{..}
	| "fig:" `isPrefixOf` abbr =
		if page == FullPage || abbr `elem` (figureAbbr . snd . figures page) then "#" ++ urlChars abbr
		else remoteFigureHref (figureByAbbr draft abbr)
	| "tab:" `isPrefixOf` abbr =
		case tableByAbbr draft abbr of
			Just t | page /= FullPage, not ([abbr] `elem` (tableAbbrs . snd . tables page)) -> remoteTableHref t
			_ -> "#" ++ urlChars abbr
	| abbrIsOnPage abbr page = "#" ++ case page of
	    SectionPage sec -> urlChars (parentLink sec abbr)
	    _ -> urlChars abbr
	| otherwise = linkToSectionHref SectionToSection abbr

prepMath :: LaTeX -> String
prepMath = Text.unpack . renderLaTeX . (>>= cleanup)
  where
    cleanupText :: LaTeX -> LaTeX -- MathJax does not support \, in \text
    cleanupText [] = []
    cleanupText (TeXComm "," [] : x) = TeXRaw " " : cleanupText x
    cleanupText (x : y) = cleanup x ++ cleanupText y

    cleanup :: LaTeXUnit -> LaTeX
    cleanup (TeXComm "texttt" [(FixArg, [TeXComm "textit" x])]) =
        [TeXComm "class" [(FixArg, [TeXRaw "textit"]), (FixArg, [TeXComm "texttt" x])]]
        -- MathJax does not support \textit inside \texttt
    cleanup (TeXComm "tcode" x) = cleanup (TeXComm "texttt" (map (second (>>= cleanup)) x))
    cleanup (TeXComm "nontcode" x) = [TeXComm "texttt" (map (second (>>= cleanup)) x)]
    cleanup (TeXComm "ensuremath" [(FixArg, x)]) = x >>= cleanup
    cleanup (TeXComm "discretionary" _) = []
    cleanup (TeXComm "hfill" []) = []
    cleanup (TeXComm "text" [(FixArg, x)]) = [TeXComm "text" [(FixArg, cleanupText x)]]
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
renderMath [TeXMath Dollar (c@(TeXComm "noncxxtcode" _) : more)] ctx =
  render c ctx ++ renderMath [TeXMath Dollar more] ctx
renderMath m ctx
	| isComplexMath m = renderComplexMath (mapTeX replaceNonCxxTcode m) ctx
	| otherwise = spanTag s $ renderSimpleMath m ctx
	where
		s = mathKind m
		mathKind [TeXMath Square _] = "mathblock"
		mathKind _ = "math"
		replaceNonCxxTcode :: LaTeXUnit -> Maybe LaTeX
		replaceNonCxxTcode (TeXComm "noncxxtcode" args) = Just [TeXComm "tcode" args]
		replaceNonCxxTcode _ = Nothing

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
renderSimpleMathUnit (TeXComm "mathtt" [(FixArg, x)]) ctx = spanTag "mathtt" (highlight ctx x)
renderSimpleMathUnit (TeXBraces x) sec = renderSimpleMath x sec
renderSimpleMathUnit (TeXMath Dollar m) sec = renderSimpleMath (trim m) sec
renderSimpleMathUnit (TeXMath _ m) sec = renderSimpleMath m sec
renderSimpleMathUnit other sec = render other sec

mathKey :: LaTeX -> (String, Bool)
mathKey m = case m of
		[TeXMath kind t] -> (prepMath t, kind == Dollar)
		[TeXEnv "eqnarray*" [] _] -> (prepMath m, False)
		_ -> (prepMath m, True)

highlightCodeInMath :: RenderContext -> [Soup.Tag Text] -> TextBuilder.Builder
highlightCodeInMath ctx
    ( open@(Soup.TagOpen "span" (("class", cls) : _))
    : Soup.TagText code
    : close@(Soup.TagClose "span")
    : more )
      | cls `elem` ["mjx-char MJXc-TeX-type-R", "mjx-charbox MJXc-TeX-type-R"]
        = TextBuilder.fromText (Soup.renderTags [open])
        ++ highlight ctx [TeXRaw code]
        ++ TextBuilder.fromText (Soup.renderTags [close])
        ++ highlightCodeInMath ctx more
highlightCodeInMath ctx (a:b) = TextBuilder.fromText (Soup.renderTags [a]) ++ highlightCodeInMath ctx b
highlightCodeInMath _ [] = ""

{- Unfortunately, for:    \class{hidden_link}{\href{url}{bla}}
MathJax generates:        <a href="url"><span class="yada hidden_link">bla</span></a>

But CSS does not let you say "apply the following style to 'a' elements that have a 'span' child with class 'hidden_link'".

So fixHiddenLinks moves the "hidden_link" class from the span to the a... -}

fixHiddenLinks :: [Soup.Tag Text] -> [Soup.Tag Text]
fixHiddenLinks (Soup.TagOpen "a" attrs : Soup.TagOpen "span" [("class", Text.words -> cls)] : rest)
    | "hidden_link" `elem` cls
        = Soup.TagOpen "a" (("class", "hidden_link") : attrs) :
          Soup.TagOpen "span" [("class", Text.unwords $ cls \\ ["hidden_link"])] : rest
fixHiddenLinks (x:y) = x : fixHiddenLinks y
fixHiddenLinks [] = []

removeAriaLabel :: Soup.Tag Text -> Soup.Tag Text
removeAriaLabel (Soup.TagOpen x attrs) = Soup.TagOpen x (filter ((/= "aria-label") . fst) attrs)
removeAriaLabel x = x

renderComplexMath :: LaTeX -> RenderContext -> TextBuilder.Builder
renderComplexMath math ctx
    | inline = html
    | otherwise = "<br>" ++ html
    where
        (formula, inline) = mathKey math
        html = highlightCodeInMath ctx $ fixHiddenLinks $ map removeAriaLabel $ Soup.parseTags $ MathJax.render formula inline

renderTable :: LaTeX -> [Row [TeXPara]] -> RenderContext -> TextBuilder.Builder
renderTable colspec a sec =
	xml "table" [] (renderRows (parseColspec colspec) a)
	where
		parseColspec :: LaTeX -> [Text]
		parseColspec [] = []
		parseColspec (TeXRaw (Text.unpack -> '|' : x) : y) = go (TeXRaw (Text.pack x) : y)
		parseColspec x = go x

		go :: LaTeX -> [Text]
		go [] = []
		go [TeXRaw "|"] = []
		go (TeXRaw "@" : TeXBraces _ : x) = go x -- unimplemented
		go (TeXRaw ">" : TeXBraces _ : x) = go x -- unimplemented
		go (TeXRaw "" : y) = go y
		go (TeXRaw (Text.unpack -> letter : rest) : y)
		    | letter == ' ' = go (TeXRaw (Text.pack rest) : y)
		    | letter == '|' = (\(d:b)->("border " ++ d:b)) $ go (TeXRaw (Text.pack rest) : y)
		    | otherwise = colClass letter : go (TeXRaw (Text.pack rest) : y)
		go (TeXBraces _ : x) = go x -- unimplemented
		go x = error ("parseColspec: " ++ show x)
		
		colClass :: Char -> Text
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
					[c''] = parseColspec cs'
					c' = combine c'' c ++ clineClass colnum clines
					colspan
						| null rest = length cs + 1
						| otherwise = w
				in
					renderCell colspan c' (renderLatexParas content sec)
					++ renderCols (drop (colspan - 1) cs) (colnum + colspan) clines rest
			| otherwise =
				renderCell 1 (c ++ clineClass colnum clines) (renderLatexParas content sec)
				++ renderCols cs (colnum + 1) clines rest
		renderCols [] _ _ (_ : _) = error "Too many columns"

		clineClass n clines
			| isJust $ find (\(begin, end) -> begin <= n && n <= end) clines =
				" cline"
			| otherwise = ""

renderCell :: Int -> Text -> TextBuilder.Builder -> TextBuilder.Builder
renderCell colspan classes content = xml "td" attrs content
    where
        classes' = if TextBuilder.toLazyText content == "" then "empty " ++ classes else classes
        attrs = [("colspan", Text.pack $ show colspan) | colspan /= 1]
            ++ [("class", classes')]

instance Render TeXPara where
	render = (mconcat .) . (intersperse " " .) . mapM render . sentences

instance Render [Element] where
    render l@(LatexElement _ : _) ctx = render (spanJust l p) ctx
        where
            p (LatexElement e) = Just e
            p _ = Nothing
    render (x : y) ctx = render x ctx ++ render y ctx
    render [] _ = ""

instance Render Sentence where
	render Sentence{..} ctx
			| (Enumerated _ _ : _) <- sentenceElems = render sentenceElems ctx -- not a real sentence
			| not (inSentence ctx), Just v <- i =
					xml "div" [("id", v), ("class", "sentence")] $
						render (case linkifyFullStop link sentenceElems of Just x -> x; Nothing -> sentenceElems) ctx{inSentence = True}
			| otherwise = render sentenceElems ctx
		where
			i = case sentenceNumber of
				Just v -> Just $ idPrefix ctx ++ "sentence-" ++ Text.pack (show v)
				Nothing -> Nothing
			link = TeXComm "class"
			    [ (FixArg, [TeXRaw "hidden_link"])
			    , (FixArg, [TeXComm "href" [(FixArg, [TeXRaw ("#" ++ fromJust i)]), (FixArg, [TeXRaw "."])]])
			    ] -- in math, \class and \href are recognized by mathjax

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

grammarNameRef :: Abbreviation -> Text -> RenderContext -> Text
grammarNameRef section name RenderContext{..} =
    (if abbrIsOnPage section page then "" else "SectionToSection/" ++ section)
    ++ "#nt:" ++ (Text.toLower name)

data Link = TocToSection | SectionToToc | SectionToSection
	deriving Show

linkToSectionHref :: Link -> Abbreviation -> Text
linkToSectionHref link abbr = Text.pack (show link) ++ "/" ++ urlChars abbr

linkToSection :: Link -> Abbreviation -> Anchor
linkToSection link abbr = anchor{ aHref = linkToSectionHref link abbr, aText = squareAbbr abbr }

--url :: Text -> Text
--url = urlChars . LazyText.toStrict . TextBuilder.toLazyText . flip render defaultRenderContext{replXmlChars = False}

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

fileContent :: TextBuilder.Builder -> TextBuilder.Builder -> TextBuilder.Builder -> TextBuilder.Builder -> TextBuilder.Builder
fileContent pathHome title extraHead body =
	"<!DOCTYPE html>" ++
	"<html lang='en'>" ++
		"<head>" ++
			"<title>" ++ title ++ "</title>" ++
			"<meta charset='UTF-8'/>" ++
			"<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "14882.css'/>" ++
			"<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "expanded.css' title='Normal'/>" ++
			"<link rel='alternate stylesheet' type='text/css' href='" ++ pathHome ++ "colored.css' title='Notes and examples colored'/>" ++
			"<link rel='alternate stylesheet' type='text/css' href='" ++ pathHome ++ "normative-only.css' title='Notes and examples hidden'/>" ++
			"<link rel='icon' href='icon.png'/>" ++
			extraHead ++
		"</head>" ++
		"<body><div class='wrapper'>" ++ body ++ "</div></body>" ++
	"</html>"

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
