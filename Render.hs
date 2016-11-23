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
	fileContent, Link(..), outputDir, linkToRemoteTable,
	abbrAsPath, abbreviations, imgDir, RenderContext(..),
	) where

import Load14882 (
	CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Elements, Draft,
	Section(..), Chapter(..), Table(..), Figure(..), figures, tables,
	IndexComponent(..), IndexTree, IndexNode(..), IndexKind(..), IndexEntry(..), parseIndex,
	IndexPath, indexKeyContent, tableByAbbr, figureByAbbr)

import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), MathType(..), matchCommand, matchEnv)
import qualified Text.LaTeX.Base.Render as TeXRender
import Data.Text (isPrefixOf)
import qualified Data.Text as Text
import Data.Char (isAlpha, isSpace)
import Control.Monad (when, liftM2)
import qualified Prelude
import Prelude hiding (take, last, (.), (++), writeFile)
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import System.Directory (doesFileExist)
import Data.Hashable (hash)
import Data.List (find, nub)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Util

kill, literal :: [String]
kill = ["clearpage", "renewcommand", "brk", "newcommand", "enlargethispage", "noindent", "indent", "vfill", "pagebreak", "topline", "xspace", "!", "linebreak", "caption", "capsep", "continuedcaption", "bottomline", "-", "hline", "rowsep", "hspace", "ttfamily", "endlist", "cline", "itcorr", "label"]
literal = [" ", "#", "{", "}", "~", "%", ""]

texFromArg :: TeXArg -> LaTeX
texFromArg (FixArg t) = t
texFromArg (OptArg t) = t
texFromArg (SymArg t) = t
texFromArg _ = error "no"

simpleMacros :: [(String, Text)]
simpleMacros =
	[ ("dcr"            , "--")
	, (","              , "&nbsp;")
	, ("\""             , "\"")
	, ("prime"          , "'")
	, ("caret"          , "^")
	, ("atsign"         , "@")
	, ("copyright"      , "&copy;")
	, ("textregistered" , "&reg;")
	, ("Cpp"            , "C++")
	, ("cppver"         , "201402L")
	, ("sum"            , "∑")
	, ("ell"            , "ℓ")
	, ("shr"            , ">>")
	, ("cv"             , "cv")
	, ("shl"            , "&lt;&lt;")
	, ("br"             , "<br/>&emsp;")
	, ("sim"            , "~")
	, ("quad"           , "&emsp;&ensp;")
	, ("indent"         , "&emsp;")
	, ("unun"           , "__")
	, ("^"              , "^")
	, ("ldots"          , "&hellip;")
	, ("times"          , "&times;")
	, ("&"              , "&amp;")
	, ("$"              , "&#36;")
	, ("backslash"      , "\\")
	, ("textbackslash"  , "\\")
	, ("textunderscore" , "_")
	, ("colcol"         , "::")
	, ("tilde"          , "~")
	, ("hspace"         , " ")
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
	, ("dotAt"          , ".")
	, ("atDot"          , ".")
	, ("textlangle"     , "&langle;")
	, ("textrangle"     , "&rangle;")
	]
	++ [(n, Text.pack [c]) | (n, c) <- greekAlphabet]

makeSpan, makeDiv, makeBnfTable, makeBnfPre :: [String]
makeSpan = words "center"
makeDiv = words "definition cvqual textit textnormal emph exitnote footnote terminal nonterminal mathit indented paras ttfamily TableBase table tabular longtable"
makeBnfTable = words "bnfkeywordtab bnftab ncbnftab"
makeBnfPre = words "bnf ncbnf simplebnf ncsimplebnf"

indexPathString :: IndexPath -> Text
indexPathString =
	replace " " "_" . -- HTML forbids space.
	replace "~" " " . -- The LaTeX uses ~ erratically, e.g. "\indextext{ambiguity!declaration~versus cast}"
	Text.intercalate "," .
	map (indexKeyContent . indexKey)

urlChars :: Text -> Text
urlChars =
	replace "'"  "&#39;" .
	replace "<"  "%3c" .
	replace ">"  "%3e" .
	replace "\"" "%22" .
	replace "#"  "%23" .
	replace "{"  "%7b" .
	replace "|"  "%7c" .
	replace "}"  "%7d" .
	replace "^"  "%5e" .
	replace " "  "%20" .
	replace "%"  "%25"

indexPathId :: IndexPath -> Text
indexPathId =
	replace " "  "%20" .
	replace "'" "&#39;" .
	indexPathString

indexPathHref :: IndexPath -> Text
indexPathHref = urlChars . indexPathString

instance Render Anchor where
	render Anchor{..} _ = xml "a" ([("class", aClass) | aClass /= "" ] ++
	                             [("href" , aHref ) | aHref  /= "" ] ++
	                             [("id"   , aId   ) | aId    /= "" ] ++
	                             [("style", aStyle) | aStyle /= "" ])
	                        aText


class Render a where render :: a -> RenderContext -> Text

instance Render a => Render [a] where
	render = mconcat . map render

instance (Render a, Render b) => Render (a, b) where
	render (x, y) = render x ++ render y

instance Render TeXArg where
	render = render . texFromArg

instance Render LaTeX where
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
	                                   $ replace ">" "&gt;"
	                                   $ replace "<" "&lt;"
	                                   $ replace "&" "&amp;"
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
	render (TeXComm "ref" [FixArg abbr]) = \RenderContext{..} -> simpleRender (case () of
		_ | "fig:" `isPrefixOf` simpleRender abbr ->
			if abbr `elem` (figureAbbr . figures page) then anchor{aHref = "#" ++ url abbr}
			else linkToRemoteFigure (figureByAbbr draft abbr)
		_ | "tab:" `isPrefixOf` simpleRender abbr ->
			case tableByAbbr draft abbr of
				Just t | not ([abbr] `elem` (tableAbbrs . tables page)) -> linkToRemoteTable t
				_ -> anchor{aHref = "#" ++ url abbr}
		_ -> linkToSection SectionToSection abbr){aText = squareAbbr abbr}
	render (TeXComm "nontermdef" [FixArg (TeXRaw s)]) = render anchor{aId = "nt:"++s, aText = s++":"}
	render (TeXComm "grammarterm_" ((FixArg (TeXRaw section)) : (FixArg (TeXRaw name)) : otherArgs)) =
		\sec ->
		xml "i" [] $ render anchor{aHref=grammarNameRef section name, aText=name ++ render otherArgs sec} sec
	render (TeXComm "bigoh" [FixArg content]) =
		spanTag "math" . ("Ο(" ++) . (++ ")") . renderMath content
	render (TeXComm "texttt" [FixArg x]) = \ctx -> "<span class='texttt'>" ++ render x ctx{rawHyphens=True} ++ "</span>"
	render (TeXComm "textit" [FixArg x]) = ("<i>" ++) . (++ "</i>") . render x
	render (TeXComm "textit" [FixArg x, OptArg y]) = \sec -> "<i>" ++ render x sec ++ "</i>[" ++ render y sec ++ "]"
	render (TeXComm "textbf" [FixArg x]) = ("<b>" ++) . (++ "</b>") . render x
	render (TeXComm "index" [OptArg _, FixArg (parseIndex -> (p, _))])
		= spanTag "indexparent" . render anchor{aId=indexPathId p, aClass="index"}
	render (TeXComm "defnx" (FixArg x : FixArg (parseIndex -> (p, _)) : y))
		= \sec -> render anchor
			{ aText  = "<i>" ++ render x sec ++ "</i>"
			, aId    = "def:" ++ indexPathId p
			, aHref  = "#def:" ++ indexPathHref p
			, aClass = "hidden_link" } sec
			++ render y sec
	render (TeXComm "multicolumn" [FixArg (TeXRaw n), _, FixArg content]) = xml "td" [("colspan", n)] . render content
	render (TeXComm "leftshift" [FixArg content]) =
		(spanTag "mathsf" "lshift" ++) . xml "sub" [("class", "math")] . render content
	render (TeXComm "verb" [FixArg a]) = \c -> xml "code" [] $ render a c{rawTilde=True, rawHyphens=True}
	render (TeXComm "footnoteref" [FixArg (TeXRaw n)]) =
		render anchor{aClass="footnotenum", aText=n, aHref="#footnote-" ++ n}
	render (TeXComm "raisebox" args)
		| FixArg (TeXRaw d) <- head args
		, FixArg content <- Prelude.last args =
			let neg s
				| Text.head s == '-' = Text.tail s
				| otherwise = "-" ++ s
			in xml "span" [("style", "position: relative; top: " ++ neg d)] . render content
	render (TeXComm "term" [FixArg x]) =
		\sec ->
			let
				y = render x sec
				i = "def:" ++ replace " " "_" y
				-- It's tempting to use 'term:' instead of 'def:' here, but if we do that,
				-- URLs break when upstream promotes a \term to a \defn.
			in render anchor
				{ aText  = "<i>" ++ y ++ "</i>"
				, aId    = i
				, aHref  = "#" ++ urlChars i
				, aClass = "hidden_link" } sec
	render (TeXComm x s)
	    | x `elem` kill                = return ""
	    | null s, Just y <-
	       lookup x simpleMacros       = return y
	    | [FixArg z] <- s, Just y <-
	       lookup x simpleMacros       = (y ++) . render z
	    | otherwise                    = spanTag (Text.pack x) . render (map texFromArg s)
	render (TeXCommS s)
	    | s `elem` literal             = return $ Text.pack s
	    | Just x <-
	       lookup s simpleMacros       = return x
	    | s `elem` kill                = return ""
	    | otherwise                    = return $ spanTag (Text.pack s) ""
	render (TeXEnv "itemdecl" [] t)    = \c -> xml "code" [("class", "itemdecl")] $ render t c{rawTilde=True, rawHyphens=True}
	render env@(TeXEnv e _ t)
	    | e `elem` makeSpan            = spanTag (Text.pack e) . render t
	    | e `elem` makeDiv             = xml "div" [("class", Text.pack e)] . render t
	    | isComplexMath env            = return $ renderComplexMath env
	    | otherwise                    = error $ "render: unexpected env " ++ e

instance Render Int where render = return . Text.pack . show

instance Render IndexComponent where
	render IndexComponent{..} =
		render (if indexFormatting == TeXEmpty then indexKey else indexFormatting)

instance Render IndexEntry where
	render IndexEntry{indexEntryKind=Just (See also x), ..} = \ctx ->
		"<i>" ++ (if also then "see also" else "see") ++ "</i> " ++
		render (anchor
			{ aHref = "#" ++
				(replace "'" "&#39;" $
				replace " " "_" $
				replace "~" " " $
				replace ", " "," $
				indexKeyContent x)
			, aText = render x ctx}) ctx
	render IndexEntry{indexEntryKind=Just IndexClose} = return ""
	render IndexEntry{indexEntryKind=Just DefinitionIndex, ..} =
		return $ simpleRender anchor
			{ aHref = "SectionToSection/" ++ url abbr ++ "#def:" ++ indexPathHref indexPath
			, aText = squareAbbr abbr }
			where abbr = abbreviation indexEntrySection
	render IndexEntry{..} = return $ simpleRender anchor
		{ aHref = "SectionToSection/" ++ url abbr ++ "#" ++ indexPathHref indexPath
		, aText = squareAbbr abbr }
		where abbr = abbreviation indexEntrySection

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
					xml "div" [("id", indexPathId up')] $
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

renderListItem :: RenderContext -> Int -> Elements -> Text
renderListItem ctx n elems =
		xml "li" [("id", thisId)] $
		(xml "div" [("class", "marginalizedparent"), ("style", "left:" ++ left)]
			(render (anchor{
				aClass = "marginalized",
				aHref  = "#" ++ thisId,
				aText  = "(" ++ mconcat (map (\x -> simpleRender x ++ ".") (paragraph ctx))
						++ simpleRender n ++ ")"
			}) ctx') ++)
		(render elems ctx')
	where
		left = simpleRender (-5 - 2 * length (paragraph ctx)) ++ "em"
		thisId = idPrefix ctx ++ simpleRender n
		ctx' = ctx{ idPrefix = thisId ++ ".", paragraph = paragraph ctx ++ [n] }

instance Render Element where
	render (LatexElements t) = \sec ->
		case Text.stripStart (render t sec) of "" -> ""; x -> xml "p" [] x
	render (Bnf e t)
		| e `elem` makeBnfTable = renderBnfTable t
		| e `elem` makeBnfPre = bnfPre . render (preprocessPre t)
		| otherwise = error "unexpected bnf"
	render (TableElement t) = renderTab False t
	render (Tabbing t) =
		xml "pre" [] . htmlTabs . render (preprocessPre t)
	render (FigureElement f) = return $ renderFig False f
	render Codeblock{..} = \c -> xml "pre" [("class", "codeblock")] (render code c{rawTilde=True, rawHyphens=True, rawSpace=True})
	render (Enumerated ek ps) = \ctx -> xml t [] $ mconcat $ uncurry (renderListItem ctx) . (zip [1..] ps)
		where
			t = case ek of
				"enumeraten" -> "ol"
				"enumeratea" -> "ol"
				"enumerate" -> "ol"
				"itemize" -> "ul"
				"description" -> "ul"
				_ -> undefined
	render (Footnote n content) = \sec ->
		let
			num = render n sec
		in
			xml "div" [("class", "footnote"), ("id", "footnote-" ++ num)] $
			xml "div" [("class", "footnoteNumberParent")]
				(render anchor{aText=num++")", aHref="#footnote-" ++ num, aClass="marginalized"} sec) ++
			render content sec
	render (Minipage content) = xml "div" [("class", "minipage")] . render content

isComplexMath :: LaTeX -> Bool
isComplexMath (TeXMath _ t) = 
	(not . null $ matchCommand (`elem` ["frac", "sum", "binom", "int"]) t)
	||
	(not . null $ matchEnv (`elem` ["array"]) t)
isComplexMath (TeXEnv e _ _) = e `elem` ["eqnarray*"]
isComplexMath _ = False

data RenderContext = RenderContext
	{ page :: Maybe Section
	, draft :: Draft
	, rawHyphens :: Bool -- in real code envs /and/ in \texttt
	, rawTilde :: Bool   -- in real code envs but not in \texttt
	, rawSpace :: Bool
	, paragraph :: [Int]
	, idPrefix :: Text }

squareAbbr :: Render a => a -> Text
squareAbbr x = "[" ++ simpleRender x ++ "]"

linkToRemoteTable :: Table -> Anchor
linkToRemoteTable Table{tableSection=Section{..}, ..} =
	anchor{ aHref = "SectionToSection/" ++ url abbreviation ++ "#" ++ url (head tableAbbrs) }

linkToRemoteFigure :: Figure -> Anchor
linkToRemoteFigure Figure{figureSection=Section{..}, ..} =
	anchor{ aHref = "SectionToSection/" ++ url abbreviation ++ "#" ++ url figureAbbr }

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
renderSimpleMath (TeXMath _ m) sec = renderSimpleMath m sec
renderSimpleMath other sec = render other sec

renderComplexMath :: LaTeX -> Text
renderComplexMath m =
	unsafePerformIO $ do
	exists <- doesFileExist filePath
	when (not exists) generateImage
	return $ "<img src='ToImage/" ++ Text.pack fileName ++ "' class='" ++ imgClass m ++ "' alt='" ++ escape math ++ "'/>"

	where
		imgClass (TeXMath Square _) = "mathblockimg"
		imgClass (TeXMath _ _) = "mathinlineimg"
		imgClass _ = "mathblockimg"

		generateImage =
			withSystemTempDirectory "genhtml" $ \tmp -> do
			_ <- readProcess "latex" ["-output-format=dvi", "-output-directory=" ++ tmp, "-halt-on-error"] latex
			_ <- readProcess "dvipng" ["-bg", "transparent", "-T", "tight", "-D", "130", tmp ++ "/texput.dvi", "-o", filePath] ""
			return ()

		escape = replace "'" "&apos;" . replace "&" "&amp;"

		math = TeXRender.render m
		fileName = (show . abs $ hash math) ++ ".png"
		filePath = outputDir ++ imgDir ++ fileName
		latex = Text.unpack $
			"\\documentclass{article}\n" ++
			"\\pagestyle{empty}\n" ++
			"\\usepackage{array}\n" ++
			"\\usepackage{amsmath}\n" ++
			"\\usepackage{mathrsfs}\n" ++
			"\\usepackage[T1]{fontenc}\n" ++
			"\\begin{document}\n"
			++
			math
			++
			"\\end{document}\n"

renderTable :: LaTeX -> [Row Elements] -> RenderContext -> Text
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

renderCell :: Elements -> RenderContext -> Text
renderCell e sec = mconcat (map renderCell' e)
	where
		renderCell' (LatexElements t) = render t sec
		renderCell' other = render other sec

-- Explicit <br/>'s are redundant in <pre>, so strip them.
preprocessPre :: LaTeX -> LaTeX
preprocessPre (TeXLineBreak _ _) = TeXEmpty
preprocessPre (TeXCommS "br") = TeXEmpty
preprocessPre (TeXEnv e a c) = TeXEnv e a (preprocessPre c)
preprocessPre (TeXSeq a b) = TeXSeq (preprocessPre a) (preprocessPre b)
preprocessPre rest = rest

bnfPre :: Text -> Text
bnfPre = xml "pre" [("class", "bnf")] . Text.strip

htmlTabs :: Text -> Text
htmlTabs = replace "\t" "&#9;"

renderBnfTable :: LaTeX -> RenderContext -> Text
renderBnfTable l = bnfPre . htmlTabs . render (preprocessPre l)

grammarNameRef :: Text -> Text -> Text
grammarNameRef s n = "SectionToSection/" ++ s ++ "#nt:" ++ (Text.toLower n)

data Link = TocToSection | SectionToToc | SectionToSection | ToImage
	deriving Show

linkToSection :: Link -> LaTeX -> Anchor
linkToSection link abbr = anchor
	{	aHref = Text.pack (show link) ++ "/" ++ url abbr
	,	aText = squareAbbr abbr }

url :: LaTeX -> Text
url = replace "&lt;" "%3c"
    . replace "&gt;" "%3e"
    . simpleRender

simpleRender :: Render a => a -> Text
simpleRender = flip render (RenderContext (error "no page") (error "no draft") False False False [] "")

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
			| otherwise = Text.pack [['A'..] !! (head ns - 31)] -- todo

abbreviations :: Section -> [LaTeX]
abbreviations Section{..} = abbreviation : concatMap abbreviations subsections

fileContent :: Text -> Text -> Text -> Text
fileContent pathHome title body =
	"<!DOCTYPE html>" ++
	"<html lang='en'>" ++
		"<head>" ++
			"<title>" ++ title ++ "</title>" ++
			"<meta charset='UTF-8'/>" ++
			"<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "14882.css'/>" ++
		"</head>" ++
		"<body><div class='wrapper'>" ++ body ++ "</div></body>" ++
	"</html>"

abbrAsPath :: LaTeX -> Text
abbrAsPath
	= replace "&lt;" "<"
	. replace "&gt;" ">"
	. simpleRender

data SectionFileStyle = Bare | WithExtension | InSubdir
	deriving (Eq, Read)

doLink :: SectionFileStyle -> Link -> Text -> Text
doLink sfs l = go . Text.splitOn (Text.pack (show l) ++ "/")
	where
		go (x : (Text.break (`elem` ("'#" :: String)) -> (a, b)) : z) = x ++ f a ++ go (b : z)
		go [x] = x
		go _ = undefined
		idir = Text.pack imgDir
		f :: Text -> Text
		f u = case (sfs, l) of
			(Bare, SectionToToc) -> "./#" ++ u
			(Bare, TocToSection) -> dotSlashForColon u
			(Bare, SectionToSection) -> dotSlashForColon u
			(Bare, ToImage) -> idir ++ u
			(InSubdir, SectionToToc) -> "../#" ++ u
			(InSubdir, TocToSection) -> u ++ "/"
			(InSubdir, SectionToSection) -> "../" ++ u
			(InSubdir, ToImage) -> "../" ++ idir ++ u
			(WithExtension, SectionToToc) -> "index.html#" ++ u
			(WithExtension, TocToSection) -> dotSlashForColon u ++ ".html"
			(WithExtension, SectionToSection) -> dotSlashForColon u ++ ".html"
			(WithExtension, ToImage) -> idir ++ u
		dotSlashForColon x = if ':' `elem` Text.unpack x then "./" ++ x else x
			-- Without this, we generate urls like "string::replace.html",
			-- in which "string" is parsed as the protocol.

applySectionFileStyle :: SectionFileStyle -> Text -> Text
applySectionFileStyle sfs =
	doLink sfs SectionToSection
	. doLink sfs SectionToToc
	. doLink sfs TocToSection
	. doLink sfs ToImage

outputDir, imgDir :: FilePath
outputDir = "14882/"
imgDir = "math/"
