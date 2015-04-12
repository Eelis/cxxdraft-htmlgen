{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	TupleSections,
	ViewPatterns,
	LambdaCase #-}

module Render (
	Render(render), url, renderTab, renderFig,
	linkToSection, secnum, SectionFileStyle(..), applySectionFileStyle,
	fileContent, Link(..), outputDir,
	abbrAsPath, abbreviations, imgDir
	) where

import Load14882 (
	CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Paragraph,
	Section(..), Chapter(..), Table(..), Figure(..))

import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), MathType(..), matchCommand, matchEnv)
import qualified Text.LaTeX.Base.Render as TeXRender
import Data.Text (isPrefixOf)
import qualified Data.Text as Text
import Data.Char (isAlpha)
import Control.Monad (when)
import qualified Prelude
import Prelude hiding (take, last, (.), (++), writeFile)
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import System.Directory (doesFileExist)
import Data.Hashable (hash)
import Data.List (find)
import Data.Maybe (isJust)
import Util

kill, literal :: [String]
kill = ["indextext", "indexdefn", "indexlibrary", "indeximpldef", "printindex", "clearpage", "renewcommand", "brk", "newcommand", "footnotetext", "enlargethispage", "index", "noindent", "indent", "vfill", "pagebreak", "topline", "xspace", "!", "linebreak", "caption", "setcounter", "addtocounter", "capsep", "continuedcaption", "bottomline", "-", "hline", "rowsep", "hspace", "ttfamily", "endlist", "cline"]
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
	, ("prime"          , "'")
	, ("atsign"         , "@")
	, ("copyright"      , "&copy;")
	, ("textregistered" , "&reg;")
	, ("Cpp"            , "C++")
	, ("cppver"         , "201402L")
	, ("alpha"          , "α")
	, ("beta"           , "β")
	, ("delta"          , "δ")
	, ("lambda"         , "λ")
	, ("mu"             , "μ")
	, ("pi"             , "π")
	, ("rho"            , "ρ")
	, ("sigma"          , "σ")
	, ("Gamma"          , "Γ")
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
	]

makeSpan, makeDiv, makeBnfTable, makeBnfPre :: [String]
makeSpan = words "indented itemdescr minipage center"
makeDiv = words "defn definition cvqual tcode textit textnormal term emph grammarterm exitnote footnote terminal nonterminal mathit enternote exitnote enterexample exitexample indented paras ttfamily TableBase table tabular longtable"
makeBnfTable = words "bnfkeywordtab bnftab"
makeBnfPre = words "bnf simplebnf"

instance Render Anchor where
	render Anchor{..} = xml "a" ([("class", aClass) | aClass /= "" ] ++
	                             [("href" , aHref ) | aHref  /= "" ] ++
	                             [("id"   , aId   ) | aId    /= "" ])
	                        aText


class Render a where render :: a -> Text

instance Render a => Render [a] where
	render = mconcat . map render

instance (Render a, Render b) => Render (a, b) where
	render (x, y) = render x ++ render y

instance Render LaTeX where
	render (TeXSeq (TeXCommS "textbackslash") y)
		| TeXSeq (TeXRaw s) rest <- y  = "\\" ++ render (TeXRaw $ unspace s) ++ render rest
		| TeXRaw s <- y                = "\\" ++ render (TeXRaw $ unspace s)
		where
			unspace s
				| Just suffix <- Text.stripPrefix " " s = suffix
				| otherwise = s
	render (TeXSeq (TeXCommS "itshape") x) = "<i>" ++ render x ++ "</i>"
	render (TeXSeq x y               ) = render x ++ render y
	render (TeXRaw x                 ) = replace "~" " "
	                                   $ replace "--" "–"
	                                   $ replace "---" "—"
	                                   $ replace ">" "&gt;"
	                                   $ replace "<" "&lt;"
	                                   $ replace "&" "&amp;"
	                                   $ x
	render (TeXComment _             ) = ""
	render (TeXCommS "br"            ) = "<br/>"
	render (TeXLineBreak _ _         ) = "<br/>"
	render (TeXCommS "break"         ) = "<br/>"
	render (TeXEmpty                 ) = ""
	render (TeXBraces t              ) = render t
	render m@(TeXMath _ _            ) = renderMath m
	render (TeXComm "ensuremath" [FixArg x]) = renderMath x
	render (TeXComm "ref" [FixArg abbr])
		| "fig:" `isPrefixOf` render abbr || "tab:" `isPrefixOf` render abbr =
			render anchor{
				aHref = "#" ++ replace ":" "-" (render abbr),
				aText = "[" ++ render abbr ++ "]"}
		| otherwise = render $ linkToSection SectionToSection abbr
	render (TeXComm "impldef" _) = "implementation-defined"
	render (TeXComm "impdefx" [FixArg _description_for_index]) = "implementation-defined"
	render (TeXComm "xname" [FixArg (TeXRaw s)]) = spanTag "texttt" $ "_<span class=\"ungap\"></span>_" ++ s
	render (TeXComm "mname" [FixArg (TeXRaw s)]) = spanTag "texttt" $ "_<span class=\"ungap\"></span>_" ++ s ++ "_<span class=\"ungap\"></span>_"
	render (TeXComm "nontermdef" [FixArg (TeXRaw s)]) = spanTag "nontermdef" s ++ ":"
	render (TeXComm "bigoh" [FixArg content]) =
		spanTag "math" $ "𝓞(" ++ renderMath content ++ ")"
	render (TeXComm "defnx" [FixArg a, FixArg _description_for_index]) = render a
	render (TeXComm "texttt" [FixArg x]) = "<code>" ++ render x ++ "</code>"
	render (TeXComm "textit" [FixArg x]) = "<i>" ++ render x ++ "</i>"
	render (TeXComm "textit" [FixArg x, OptArg y]) = "<i>" ++ render x ++ "</i>[" ++ render y ++ "]"
	render (TeXComm "textbf" [FixArg x]) = "<b>" ++ render x ++ "</b>"
	render (TeXComm "label" [FixArg (TeXRaw x)]) = render anchor{aId=x}
	render (TeXComm "multicolumn" [FixArg (TeXRaw n), _, FixArg content]) = xml "td" [("colspan", n)] $ render content
	render (TeXComm "leftshift" [FixArg content]) =
		spanTag "mathsf" "lshift" ++ xml "sub" [("class", "math")] (render content)
	render (TeXComm "state" [FixArg a, FixArg b]) =
		spanTag "tcode" (render a) ++ xml "sub" [("class", "math")] (render b)
	render (TeXComm "verb" [FixArg a]) = xml "code" [] $ renderVerb a
	render (TeXComm "footnoteref" [FixArg (TeXRaw n)]) =
		render anchor{aClass="footnotenum", aText=n, aHref="#footnote-" ++ n}
	render (TeXComm "raisebox" args)
		| FixArg (TeXRaw d) <- head args
		, FixArg content <- Prelude.last args =
			let neg s
				| Text.head s == '-' = Text.tail s
				| otherwise = "-" ++ s
			in xml "span" [("style", "position: relative; top: " ++ neg d)] $ render content
	render (TeXComm x s)
	    | x `elem` kill                = ""
	    | null s, Just y <-
	       lookup x simpleMacros       = y
	    | [FixArg z] <- s, Just y <-
	       lookup x simpleMacros       = y ++ render z
	    | otherwise                    = spanTag (Text.pack x) (render (map texFromArg s))
	render (TeXCommS s)
	    | s `elem` literal             = Text.pack s
	    | Just x <-
	       lookup s simpleMacros       = x
	    | s `elem` kill                = ""
	    | otherwise                    = spanTag (Text.pack s) ""
	render (TeXEnv "itemdecl" [] t)    = xml "pre" [("class", "itemdecl")] $ renderCode t
	render env@(TeXEnv e _ t)
	    | e `elem` makeSpan            = spanTag (Text.pack e) (render t)
	    | e `elem` makeDiv             = xml "div" [("class", Text.pack e)] (render t)
	    | isComplexMath env            = renderComplexMath env
	    | otherwise                    = error "unexpected env"

instance Render Int where render = Text.pack . show

renderTab :: Bool -> Table -> Text
renderTab stripTab Table{..} =
	xml "div" [("class", "numberedTable"), ("id", id_)] $ -- todo: multiple abbrs?
		"Table " ++ render anchor{aText = render tableNumber, aHref = "#" ++ id_} ++ " — " ++
		render tableCaption ++ "<br>" ++ renderTable columnSpec tableBody
	where
		id_ = replace "tab:" (if stripTab then "" else "tab-") $ render $ head tableAbbrs

renderFig :: Bool -> Figure -> Text
renderFig stripFig Figure{..} =
	xml "div" [("class", "figure"), ("id", id_)] $
		figureSvg ++ "<br>" ++
		"Figure " ++ render anchor{aText=render figureNumber, aHref="#" ++ id_} ++ " — " ++
		render figureName
	where id_ = replace "fig:" (if stripFig then "" else "fig-") $ render figureAbbr

instance Render Element where
	render (LatexElements t) = case render t of "" -> ""; x -> xml "p" [] x
	render (Bnf e t)
		| e `elem` makeBnfTable = renderBnfTable t
		| e `elem` makeBnfPre = bnfPre $ render $ preprocessPre t
		| otherwise = error "unexpected bnf"
	render (TableElement t) = renderTab False t
	render (Tabbing t) =
		xml "pre" [] $ htmlTabs $ render $ preprocessTabbing $ preprocessPre t
	render (FigureElement f) = renderFig False f
	render Codeblock{..} = xml "pre" [("class", "codeblock")] $ renderCode code
	render (Enumerated ek ps) = xml t [] $ mconcat $ xml "li" [] . render . ps
		where
			t = case ek of
				"enumeraten" -> "ol"
				"enumeratea" -> "ol"
				"enumerate" -> "ol"
				"itemize" -> "ul"
				"description" -> "ul"
				_ -> undefined
	render (Footnote (render -> num) content) =
		xml "div" [("class", "footnote"), ("id", "footnote-" ++ num)] $
		xml "div" [("class", "marginalizedparent")]
			(render anchor{aText=num++")", aHref="#footnote-" ++ num, aClass="marginalized"}) ++
		render content

renderVerb :: LaTeX -> Text
renderVerb t@(TeXRaw _) = renderCode t
renderVerb (TeXBraces _) = ""
renderVerb other = render other

renderCode :: LaTeX -> Text
renderCode = comments . renderOutsideAt
	where
		doRender (TeXRaw s) =
			replace "<" "&lt;"
			$ replace ">" "&gt;"
			$ replace "&" "&amp;"
			$ s
		doRender other = render other

		renderOutsideAt (TeXSeq (TeXRaw "@") b) = "@" ++ renderInsideAt b
		renderOutsideAt (TeXSeq a b) = renderOutsideAt a ++ renderOutsideAt b
		renderOutsideAt (TeXBraces x) = "{" ++ renderOutsideAt x ++ "}"
		renderOutsideAt other = doRender other 

		renderInsideAt (TeXSeq (TeXRaw "@") b) = "@" ++ renderOutsideAt b
		renderInsideAt (TeXSeq a b) = renderInsideAt a ++ renderInsideAt b
		renderInsideAt (TeXBraces x) = renderInsideAt x
		renderInsideAt other = doRender other

		mapOdd _ [] = []
		mapOdd f [only] = [f only]
		mapOdd f (first : second : rest) = (f first) : second : (mapOdd f rest)

		comments = 	
			Text.intercalate ""
		 	. mapOdd (blockComments . Text.intercalate "\n" . map commentLine . Text.lines)  -- Because unlines would emit one unfortunate extra newline
			. Text.splitOn "@"
		commentLine l =
			case Text.breakOn "//" l of
				(stuff, "") -> stuff
				(stuff, comment) -> stuff ++ (spanTag "comment" $ commentText comment)
		blockComments =
			replace "/*" "<span class='comment'>/*"
			. replace "*/" "*/</span>"

		commentText = replace "~" "&nbsp;"

isComplexMath :: LaTeX -> Bool
isComplexMath (TeXMath _ t) = 
	(not . null $ matchCommand (`elem` ["frac", "sum", "binom"]) t)
	||
	(not . null $ matchEnv (`elem` ["array"]) t)
isComplexMath (TeXEnv e _ _) = e `elem` ["eqnarray*"]
isComplexMath _ = False

renderMath :: LaTeX -> Text
renderMath m
	| isComplexMath m = renderComplexMath m
	| otherwise = spanTag s $ renderSimpleMath m
	where
		s = mathKind m
		mathKind (TeXMath Square _) = "mathblock"
		mathKind _ = "math"

renderSimpleMath :: LaTeX -> Text
renderSimpleMath (TeXRaw s) =
	case suffix of
		Just ('^', rest) -> italicise prefix ++ output "sup" rest
		Just ('_', rest) -> italicise prefix ++ output "sub" rest
		_ -> italicise s
	where
		(prefix, suffix') = Text.break (`elem` ['^', '_']) s
		suffix = Text.uncons suffix'

		output tag rest =
			case Text.uncons rest of
				Just (c, rest') -> xml tag [] (italicise $ Text.singleton c) ++ (renderSimpleMath $ TeXRaw rest')
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
renderSimpleMath (TeXSeq (TeXRaw s) rest)
	| last `elem` ["^", "_"] =
		renderSimpleMath (TeXRaw $ Text.reverse $ Text.drop 1 s')
		++ xml tag [] (renderSimpleMath content)
		++ renderSimpleMath rest'
	| otherwise = renderSimpleMath (TeXRaw s) ++ renderSimpleMath rest
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
renderSimpleMath (TeXBraces x) = renderSimpleMath x
renderSimpleMath (TeXSeq (TeXComm "frac" [(FixArg num)]) rest) =
	"[" ++ renderSimpleMath num ++ "] / [" ++ renderSimpleMath den ++ "]" ++ renderSimpleMath rest'
	where
		(den, rest') = findDenum rest
		findDenum (TeXSeq (TeXBraces d) r) = (d, r)
		findDenum (TeXSeq _ r) = findDenum r
		findDenum r = (r, TeXEmpty)
renderSimpleMath (TeXSeq a b) = (renderSimpleMath a) ++ (renderSimpleMath b)
renderSimpleMath (TeXMath _ m) = renderSimpleMath m
renderSimpleMath other = render other

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

renderTable :: LaTeX -> [Row Paragraph] -> Text
renderTable colspec =
	xml "table" [] .
	renderRows (parseColspec $ Text.unpack $ stripColspec colspec)
	where
		stripColspec (TeXRaw s) = s
		stripColspec (TeXSeq a b) = stripColspec a ++ stripColspec b
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
					(xml "td" [("colspan", render colspan), ("class", c')] $ renderCell content)
					++ renderCols (drop (colspan - 1) cs) (colnum + colspan) clines rest
			| otherwise =
				(xml "td" [("class", c ++ clineClass colnum clines)] $ renderCell content)
				++ renderCols cs (colnum + 1) clines rest
		renderCols [] _ _ (_ : _) = error "Too many columns"

		clineClass n clines
			| isJust $ find (\(begin, end) -> begin <= n && n <= end) clines =
				" cline"
			| otherwise = ""

renderCell :: Paragraph -> Text
renderCell = mconcat . map renderCell'
	where
		renderCell' (LatexElements t) = render t
		renderCell' other = render other

-- Explicit <br/>'s are redundant in <pre>, so strip them.
preprocessPre :: LaTeX -> LaTeX
preprocessPre (TeXLineBreak _ _) = TeXEmpty
preprocessPre (TeXCommS "br") = TeXEmpty
preprocessPre (TeXEnv e a c) = TeXEnv e a (preprocessPre c)
preprocessPre (TeXSeq a b) = TeXSeq (preprocessPre a) (preprocessPre b)
preprocessPre rest = rest

bnfPre :: Text -> Text
bnfPre = xml "pre" [("class", "bnf")] . Text.strip

preprocessTabbing :: LaTeX -> LaTeX
preprocessTabbing = go
	where
		initialTab (Text.stripPrefix ">" -> Just rest) =
			"\t" ++ rest
		initialTab other = other

		go (TeXBraces t) = t
		go (TeXSeq (TeXCommS "") (TeXSeq (TeXRaw s) rest)) =
			TeXSeq (TeXCommS "") (TeXSeq (TeXRaw $ initialTab s) $ go rest)
		go (TeXSeq (TeXCommS "") (TeXRaw s)) =
			TeXSeq (TeXCommS "") (TeXRaw $ initialTab s)
		go (TeXSeq a b) = TeXSeq (go a) (go b)
		go (TeXEnv e a c) = TeXEnv e a (go c)
		go other = other

htmlTabs :: Text -> Text
htmlTabs = replace "\t" "&#9;"

renderBnfTable :: LaTeX -> Text
renderBnfTable = bnfPre . htmlTabs . render . preprocessTabbing . preprocessPre

data Link = TocToSection | SectionToToc | SectionToSection | ToImage
	deriving Show

linkToSection :: Link -> LaTeX -> Anchor
linkToSection link abbr = anchor
	{	aHref = Text.pack (show link) ++ "/" ++ url abbr
	,	aText = "[" ++ render abbr ++ "]" }

url :: LaTeX -> Text
url = replace "&lt;" "%3c"
    . replace "&gt;" "%3e"
    . render

secnum :: Text -> Section -> Text
secnum href Section{sectionNumber=n,..} =
	render $ anchor{aClass=c, aHref=href, aText=text}
	where
		text
			| chapter == InformativeAnnex, null parents = "Annex " ++ chap ++ "&emsp;(informative)"
			| chapter == NormativeAnnex, null parents = "Annex " ++ chap ++ "&emsp;(normative)"
			| otherwise = Text.intercalate "." (chap : render . tail ns)
		ns = reverse $ n : sectionNumber . parents
		c	| chapter /= NormalChapter, null parents = "annexnum"
			| otherwise = "secnum"
		chap :: Text
		chap
			| chapter == NormalChapter = render (head ns)
			| otherwise = Text.pack [['A'..] !! (head ns - 1)]

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
	. render

data SectionFileStyle = Bare | WithExtension | InSubdir
	deriving (Eq, Read)

doLink :: SectionFileStyle -> Link -> Text -> Text
doLink sfs l = go . Text.splitOn (Text.pack (show l) ++ "/")
	where
		go (x : (Text.break (`elem` "'#") -> (a, b)) : z) = x ++ f a ++ go (b : z)
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
