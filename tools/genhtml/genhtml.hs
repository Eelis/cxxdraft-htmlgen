{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	TupleSections,
	ViewPatterns #-}

import Load14882 (Element(..), Paragraph, ChapterKind(..), Section(..), Chapter, load14882)

import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), matchCommand, lookForCommand)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO (writeFile)
import Data.Monoid (Monoid(mappend), mconcat)
import Control.Monad (forM_)
import qualified Prelude
import Prelude hiding (take, last, (.), (++), writeFile)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (createDirectoryIfMissing, copyFile)
import System.Process (readProcess)

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

(++) :: Monoid a => a -> a -> a
(++) = mappend

xml :: Text -> [(Text, Text)] -> Text -> Text
xml t attrs = (("<" ++ t ++ " " ++ Text.unwords (map f attrs) ++ ">") ++) . (++ ("</" ++ t ++ ">"))
	where
		f (n, v) = n ++ "='" ++ v ++ "'"

spanTag :: Text -> Text -> Text
spanTag = xml "span" . (:[]) . ("class",)

rangeElem :: Char -> Text -> Text -> Char -> Text
rangeElem open from to close
	= spanTag "range" $ Text.pack [open] ++ from ++ ", " ++ to ++ Text.pack [close]

h :: Maybe Text -> Int -> Text -> Text
h mc = flip xml (maybe [] ((:[]) . ("class",)) mc) . ("h" ++) . Text.pack . show

kill, literal :: [String]
kill = ["indextext", "indexdefn", "indexlibrary", "indeximpldef", "printindex", "clearpage", "renewcommand", "brk", "newcommand", "footnotetext", "enlargethispage", "index", "noindent", "indent", "vfill", "pagebreak", "topline", "xspace", "!", "linebreak", "caption", "setcounter", "addtocounter", "capsep", "continuedcaption", "bottomline"]
literal = [" ", "cv", "#", "{", "}", "-", "~", "%", ""]

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
	, ("unun"           , "__")
	, ("^"              , "^")
	, ("ldots"          , "&hellip;")
	, ("times"          , "&times;")
	, ("&"              , "&amp;")
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

makeSpan, makeDiv, makeBnfTable, makeBnfPre, makeRowsep, makeCodeblock :: [String]
makeSpan = words "ncbnf indented ncsimplebnf ttfamily itemdescr minipage center"
makeDiv = words "defn definition cvqual tcode textit textnormal term emph grammarterm exitnote footnote terminal nonterminal mathit enternote exitnote enterexample exitexample ncsimplebnf ncbnf indented paras ttfamily TableBase table tabular tabbing longtable"
makeBnfTable = words "bnfkeywordtab bnftab"
makeBnfPre = words "bnf"
makeRowsep = words "rowsep capsep hline"
makeCodeblock = words "codeblock codeblockdigitsep"

data Anchor = Anchor { aClass, aId, aHref, aText :: Text }

anchor :: Anchor
anchor = Anchor{aClass="", aId="", aHref="", aText=""}

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
	render (TeXSeq x y               ) = render x ++ render y
	render (TeXRaw x                 ) = Text.replace "~" " "
	                                   $ Text.replace "--" "–"
	                                   $ Text.replace "---" "—"
	                                   $ Text.replace ">" "&gt;"
	                                   $ Text.replace "<" "&lt;"
	                                   $ Text.replace "&" "&amp;"
	                                   $ x
	render (TeXComment _             ) = ""
	render (TeXCommS "br"            ) = "<br/>"
	render (TeXLineBreak _ _         ) = "<br/>"
	render (TeXEmpty                 ) = ""
	render (TeXBraces t              ) = render t
	render (TeXMath _ t              ) = spanTag "math" $ renderMath t
	render (TeXComm "ensuremath" [FixArg x]) = renderMath x
	render (TeXComm "ref" [FixArg x])  = render $ linkToSection "" SectionToSection x
	render (TeXComm "impldef" _) = "implementation-defined"
	render (TeXComm "impdefx" [FixArg _description_for_index]) = "implementation-defined"
	render (TeXComm "xname" [FixArg (TeXRaw s)]) = spanTag "texttt" $ "_<span class=\"ungap\"></span>_" ++ s
	render (TeXComm "mname" [FixArg (TeXRaw s)]) = spanTag "texttt" $ "_<span class=\"ungap\"></span>_" ++ s ++ "_<span class=\"ungap\"></span>_"
	render (TeXComm "nontermdef" [FixArg (TeXRaw s)]) = mconcat [spanTag "nontermdef" s, ":"]
	render (TeXComm "bigoh" [FixArg content]) = spanTag "math" $ mconcat [spanTag "mathscr" "𝓞", "(", render content, ")"]
	render (TeXComm "defnx" [FixArg a, FixArg _description_for_index]) = render a
	render (TeXComm "texttt" [FixArg x]) = "<code>" ++ render x ++ "</code>"
	render (TeXComm "textit" [FixArg x]) = "<i>" ++ render x ++ "</i>"
	render (TeXComm "textit" [FixArg x, OptArg y]) = "<i>" ++ render x ++ "</i>[" ++ render y ++ "]"
	render (TeXComm "textbf" [FixArg x]) = "<b>" ++ render x ++ "</b>"
	render (TeXComm "label" [FixArg (TeXRaw x)]) = render anchor{aId=x}
	render (TeXComm "multicolumn" [FixArg (TeXRaw n), _, FixArg content]) = xml "td" [("colspan", n)] $ render content
	render (TeXComm "leftshift" [FixArg content]) = mconcat [spanTag "mathsf" "lshift", xml "sub" [("class", "math")] $ render content]
	render (TeXComm "state" [FixArg a, FixArg b]) = mconcat [spanTag "tcode" (render a), xml "sub" [("class", "math")] $ render b]
	render (TeXComm x s)
	    | x `elem` kill                = ""
	    | null s, Just y <-
	       lookup x simpleMacros       = y
	    | [FixArg z] <- s, Just y <-
	       lookup x simpleMacros       = mconcat [y, render z]
	    | otherwise                    = spanTag (Text.pack x) (render (map texFromArg s))
	render (TeXCommS s               )
	    | s `elem` literal             = Text.pack s
	    | Just x <-
	       lookup s simpleMacros       = x
	    | s `elem` kill                = ""
	    | otherwise                    = spanTag (Text.pack s) ""
	render (TeXEnv "itemdecl" [] t)    = spanTag "itemdecl" $ Text.replace "@" "" $ render t
	render (TeXEnv e u t)
	    | e `elem` makeCodeblock       = spanTag "codeblock" $ renderCode t
	    | e `elem` makeSpan            = spanTag (Text.pack e) (render t)
	    | e `elem` makeDiv             = xml "div" [("class", Text.pack e)] (render t)
	    | otherwise                    = spanTag "poo" (Text.pack (e ++ show u ++ show t))

loadFigure :: Text -> Text
loadFigure f =
	snd $ Text.breakOn "<svg" $ Text.pack $ unsafePerformIO (readProcess "dot" ["-Tsvg", p] "")
		where p = Text.unpack $ "../../source/" ++ Text.replace ".pdf" ".dot" f

instance Render Element where
	render (LatexElements t) = xml "p" [] $ render t
	render (Bnf e t)
		| e `elem` makeBnfTable = renderBnfTable t
		| e `elem` makeBnfPre = bnf t
		| otherwise = spanTag "poo" (Text.pack (e ++ show t))
	render Table{..} =
		spanTag "tabletitle" (render tableCaption)
		++ renderTable columnSpec tableBody
	render Figure{..} =
		xml "div" [("class", "figure")] $ loadFigure figureFile ++ "<br>" ++ render figureName
	render (Enumerated ek ps) = t $ mconcat $ map (xml "li" [] . render) ps
		where
			t = case ek of
				"enumeraten" -> xml "ol" []
				"enumeratea" -> xml "ol" []
				"enumerate" -> xml "ol" []
				"itemize" -> xml "ul" []
				"description" -> xml "ul" []
				_ -> undefined

renderCode :: LaTeX -> Text
renderCode (TeXRaw s) =
	Text.replace "<" "&lt;"
	$ Text.replace ">" "&gt;"
	$ Text.replace "@" ""
	$ Text.replace "&" "&amp;"
	$ s
renderCode (TeXSeq a b) = (renderCode a) ++ (renderCode b)
renderCode (TeXBraces x) = "{" ++ (renderCode x) ++ "}"
renderCode (TeXEnv e [] x) | e `elem` makeCodeblock = renderCode x
renderCode other = render other

renderMath :: LaTeX -> Text
renderMath (TeXRaw s) =
	case suffix of
		Just ('^', rest) -> entities prefix ++ output "sup" rest
		Just ('_', rest) -> entities prefix ++ output "sub" rest
		_ -> entities s
	where
		(prefix, suffix') = Text.break (`elem` ['^', '_']) s
		suffix = Text.uncons suffix'

		output tag rest =
			case Text.uncons rest of
				Just (c, rest') -> xml tag [] (entities $ Text.singleton c) ++ (renderMath $ TeXRaw rest')
				Nothing -> error "Malformed math"
		entities =
			Text.replace "<" "&lt;"
			. Text.replace ">" "&gt;"
renderMath (TeXSeq (TeXRaw s) rest)
	| last `elem` ["^", "_"] =
		renderMath (TeXRaw $ Text.reverse $ Text.drop 1 s')
		++ xml tag [] (renderMath content)
		++ renderMath rest'
	| otherwise = renderMath (TeXRaw s) ++ renderMath rest
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
renderMath (TeXBraces x) = renderMath x
renderMath (TeXSeq (TeXComm "frac" [(FixArg num)]) rest) =
	"[" ++ renderMath num ++ "] / [" ++ renderMath den ++ "]" ++ renderMath rest'
	where
		(den, rest') = findDenum rest
		findDenum (TeXSeq (TeXBraces d) r) = (d, r)
		findDenum (TeXSeq _ r) = findDenum r
		findDenum r = (r, TeXEmpty)
renderMath (TeXSeq a b) = (renderMath a) ++ (renderMath b)
renderMath other = render other

renderTable :: LaTeX -> [[LaTeX]] -> Text
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

		rowHas f = any (\r -> not $ null $ matchCommand f r)

		renderRows _ [] = ""
		renderRows cs (row : rest)
			| rowHas (== "endfirsthead") row =
				renderRows cs r'
			| rowHas (`elem` ["caption", "bottomline"]) row =
				renderRows cs rest
			| otherwise =
				(xml "tr" cls $ renderCols cs row)
				++ renderRows cs rest
			where
				cls = if rowHas (`elem` makeRowsep) row then [("class", "rowsep")] else []
				r' = dropWhile (not . rowHas (== "endhead")) rest

		renderCols _ [] = ""
		renderCols (c : cs) (content : rest) 
			| [[FixArg (TeXRaw n), FixArg cs', FixArg content']] <- lookForCommand "multicolumn" content =
				let 
					[c''] = parseColspec $ Text.unpack $ stripColspec cs' 
					c' = combine c'' c
				in
					(xml "td" [("colspan", n), ("class", c')] $ render content')
					++ renderCols (drop ((read $ Text.unpack n) - 1) cs) rest
			| otherwise =
				(xml "td" [("class", c)] $ render content)
				++ renderCols cs rest
		renderCols [] (_ : _) = error "Too many columns"

-- Explicit <br/>'s are redundant in <pre>, so strip them.
preprocessPre :: LaTeX -> LaTeX
preprocessPre (TeXCommS "br") = TeXEmpty
preprocessPre (TeXEnv e a c) = TeXEnv e a (preprocessPre c)
preprocessPre (TeXSeq a b) = TeXSeq (preprocessPre a) (preprocessPre b)
preprocessPre rest = rest

bnf :: LaTeX -> Text
bnf = xml "pre" [("class", "bnf")] . render . preprocessPre

renderBnfTable :: LaTeX -> Text
renderBnfTable =
	xml "pre" [("class", "bnf")] . processHTML . render . preprocessTeX . preprocessPre
	where
		processHTML = Text.replace "\t" "&#9;"

		initialTab (Text.stripPrefix ">" -> Just rest) =
			"\t" ++ rest
		initialTab other = other

		preprocessTeX (TeXBraces t) = t
		preprocessTeX (TeXSeq (TeXCommS "") (TeXSeq (TeXRaw s) rest)) =
			TeXSeq (TeXCommS "") (TeXSeq (TeXRaw $ initialTab s) $ preprocessTeX rest)
		preprocessTeX (TeXSeq (TeXCommS "") (TeXRaw s)) =
			TeXSeq (TeXCommS "") (TeXRaw $ initialTab s)
		preprocessTeX (TeXSeq a b) = TeXSeq (preprocessTeX a) (preprocessTeX b)
		preprocessTeX (TeXEnv e a c) = TeXEnv e a (preprocessTeX c)
		preprocessTeX other = other

renderParagraph :: Text -> (Int, Paragraph) -> Text
renderParagraph idPrefix (show -> Text.pack -> i, x) =
	xml "div" [("class", "para"), ("id", idPrefix ++ i)] $
	xml "div" [("class", "paranumberparent")]
		(render (anchor{aClass="paranumber", aHref="#" ++ idPrefix ++ i,aText=i})) ++
	render x

data Link = TocToSection | SectionToToc | SectionToSection

linkToSection :: Text -> Link -> LaTeX -> Anchor
linkToSection clas link abbr = anchor{
		aClass = clas,
		aHref = case (sectionFileStyle, link) of
			(Bare, SectionToToc) -> "./#" ++ u -- hmm
			(Bare, TocToSection) -> u
			(Bare, SectionToSection) ->u
			(InSubdir, SectionToToc) -> "../#" ++ u
			(InSubdir, TocToSection) -> u ++ "/"
			(InSubdir, SectionToSection) -> "../" ++ u
			(WithExtension, SectionToToc) -> "index.html#" ++ u
			(WithExtension, TocToSection) -> u ++ ".html"
			(WithExtension, SectionToSection) -> u ++ ".html",
		aText  = "[" ++ render abbr ++ "]"}
	where
		u = url abbr
		url :: LaTeX -> Text
		url (TeXRaw x) = urlEncode x
		url (TeXSeq x y) = url x ++ url y
		url (TeXCommS "dcr") = "--"
		url _ = "TODO"

data SectionPath = SectionPath
	{ chapterKind :: ChapterKind
	, sectionNums :: [Int]
	}

numberSubsecs :: SectionPath -> [Section] -> [(SectionPath, Section)]
numberSubsecs (SectionPath k ns) = zip [SectionPath k (ns ++ [i]) | i <- [1..]]

renderSection :: Maybe LaTeX -> Bool -> (SectionPath, Section) -> (Text, Bool)
renderSection specific parasEmitted (path@SectionPath{..}, Section{..})
	| full = (, True) $
		xml "div" [("id", render abbreviation)] $ header ++
		xml "div" [("class", "para")] (render preamble) ++
		mconcat (map
			(renderParagraph (if parasEmitted then render abbreviation ++ "-" else ""))
			(zip [1..] paragraphs)) ++
		mconcat (fst . renderSection Nothing True . numberSubsecs path subsections)
	| not anysubcontent = ("", False)
	| otherwise =
		( header ++
		  mconcat (fst . renderSection specific False . numberSubsecs path subsections)
		, anysubcontent )
	where
		full = specific == Nothing || specific == Just abbreviation
		header = h Nothing (min 4 $ length sectionNums) $
			(if full
				then render $ anchor{
					aClass = "secnum",
					aHref  = "#" ++ render abbreviation,
					aText  = render path }
				else spanTag "secnum" (render path))
			++ render sectionName
			++ render (linkToSection "abbr_ref" (if specific == Just abbreviation then SectionToToc else SectionToSection) abbreviation)
		anysubcontent =
			or $ map (snd . renderSection specific True)
			   $ numberSubsecs path subsections


instance Render SectionPath where
	render (SectionPath k ns)
		| k == InformativeAnnex, [_] <- ns = "Annex " ++ chap ++ "&emsp;(informative)<br/>"
		| k == NormativeAnnex, [_] <- ns = "Annex " ++ chap ++ "&emsp;(normative)<br/>"
		| otherwise = Text.intercalate "." (chap : Text.pack . show . tail ns)
		where
			chap :: Text
			chap
				| k == NormalChapter = Text.pack (show (head ns))
				| otherwise = Text.pack [['A'..] !! (head ns - 1)]

abbreviations :: Section -> [LaTeX]
abbreviations Section{..} = abbreviation : concatMap abbreviations subsections

withPaths :: [Chapter] -> [(SectionPath, Section)]
withPaths chapters = f normals ++ f annexes
	where
		f :: [(ChapterKind, Section)] -> [(SectionPath, Section)]
		f x = (\(i, (k, s)) -> (SectionPath k [i], s)) . zip [1..] x
		(normals, annexes) = span ((== NormalChapter) . fst) chapters

sectionFileContent :: [Chapter] -> LaTeX -> Text
sectionFileContent chapters abbreviation =
	fileContent
		("[" ++ render abbreviation ++ "]")
		(mconcat $ fst . renderSection (Just abbreviation) False . withPaths chapters)
		(if sectionFileStyle == InSubdir then "../" else "")

tocFileContent :: [Chapter] -> Text
tocFileContent chapters =
		fileContent
			"14882: Contents"
			(mconcat (map section (withPaths chapters)))
			""
	where
		section :: (SectionPath, Section) -> Text
		section (sectionPath, Section{..}) =
			xml "div" [("id", render abbreviation)] $
			h Nothing (length $ sectionNums sectionPath) (
				xml "small" [] (
				spanTag "secnum" (render sectionPath) ++
				render (sectionName, linkToSection "abbr_ref" TocToSection abbreviation))) ++
			mconcat (map section (numberSubsecs sectionPath subsections))

fullFileContent :: [Chapter] -> Text
fullFileContent chapters =
	fileContent
		"14882"
		(mconcat $ fst . renderSection Nothing True . withPaths chapters)
		(if sectionFileStyle == InSubdir then "../" else "")

fileContent :: Text -> Text -> Text -> Text
fileContent title body pathHome =
	"<!DOCTYPE html>" ++
	"<html>" ++
		"<head>" ++
			"<title>" ++ title ++ "</title>" ++
			"<meta charset='UTF-8'/>" ++
			"<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "14882.css'/>" ++
		"</head>" ++
		"<body><div class='wrapper'>" ++ body ++ "</div></body>" ++
	"</html>"

urlEncode :: Text -> Text
urlEncode
	= Text.replace "<" "%3c"
	. Text.replace ">" "%3e"
	. Text.replace ":" "%3a"

abbrAsPath :: LaTeX -> Text
abbrAsPath (TeXRaw x) = x
abbrAsPath (TeXSeq x y) = abbrAsPath x ++ abbrAsPath y
abbrAsPath (TeXCommS "dcr") = "--"
abbrAsPath _ = "TODO"

data SectionFileStyle
	= Bare          -- e.g. intro.execution
	| WithExtension -- e.g. intro.execution.html
	| InSubdir      -- e.g. intro.execution/index.html
	deriving Eq

sectionFileStyle :: SectionFileStyle
sectionFileStyle = WithExtension

writeStuff :: [Chapter] -> IO ()
writeStuff chapters = do
	let outputDir = "14882/"
	putStr $ "Writing to " ++ outputDir; hFlush stdout

	createDirectoryIfMissing True outputDir

	copyFile "14882.css" (outputDir ++ "/14882.css")

	writeFile (outputDir ++ "/index.html") $ tocFileContent chapters

	fullFile <- case sectionFileStyle of
		Bare -> return "full"
		WithExtension -> return "full.html"
		InSubdir -> do
			createDirectoryIfMissing True (outputDir ++ "/full")
			return "full/index.html"
	writeFile (outputDir ++ fullFile) $ fullFileContent chapters

	let allAbbrs = concatMap abbreviations (snd . chapters)
	forM_ allAbbrs $ \abbreviation -> do
		putStr "."; hFlush stdout
		f <- case sectionFileStyle of
			Bare -> return $ Text.unpack $ abbrAsPath abbreviation
			WithExtension -> return $ Text.unpack $ abbrAsPath abbreviation ++ ".html"
			InSubdir -> do
				let dir = "/" ++ Text.unpack (abbrAsPath abbreviation)
				createDirectoryIfMissing True (outputDir ++ dir)
				return $ dir ++ "/index.html"
		writeFile (outputDir ++ f) $ sectionFileContent chapters abbreviation
	putStrLn $ " " ++ show (length allAbbrs) ++ " sections"

main :: IO ()
main = load14882 >>= writeStuff
