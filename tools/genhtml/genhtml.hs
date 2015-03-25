{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	TupleSections,
	ViewPatterns #-}

import Load14882 (Element(..), Paragraph, Section(..), load14882)

import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), MathType(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Monoid (Monoid(mappend), mconcat)
import Control.Monad (forM_)
import qualified Prelude
import Prelude hiding (take, last, (.), head, tail, takeWhile, (++))
import System.IO (hFlush, stdout)
import System.Directory (createDirectoryIfMissing, copyFile)

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

h :: Maybe Text -> Int -> Text -> Text
h mc = flip xml (maybe [] ((:[]) . ("class",)) mc) . ("h" ++) . Text.pack . show

kill, literal :: [String]
kill = ["indextext", "indexdefn", "indexlibrary", "indeximpldef", "printindex", "clearpage", "renewcommand", "brk", "newcommand", "footnotetext", "enlargethispage", "index", "noindent", "indent", "vfill", "pagebreak"]
literal = [" ", "cv", "#", "{", "}", "-", "~", "%", ",", ""]

texFromArg :: TeXArg -> LaTeX
texFromArg (FixArg t) = t
texFromArg (OptArg t) = t
texFromArg (SymArg t) = t
texFromArg _ = error "no"

simpleMacros :: [(String, Text)]
simpleMacros =
	[ ("cdots"         , "...")
	, ("min"           , "min")
	, ("dcr"           , "--")
	, ("prime"         , "'")
	, ("copyright"     , "&copy;")
	, ("textregistered", "&reg;")
	, ("change"        , "<br/><b>Change:</b> ")
	, ("rationale"     , "<br/><b>Rationale:</b> ")
	, ("difficulty"    , "<br/><b>Difficulty of converting:</b> ")
	, ("howwide"       , "<br/><b>How widely used:</b> ")
	, ("effect"        , "<br/><b>Effect on original feature:</b> ")
	, ("requires"      , "<i>Requires:</i> ")
	, ("throws"        , "<i>Throws:</i> ")
	, ("effects"       , "<i>Effects:</i> ")
	, ("returns"       , "<i>Returns:</i> ")
	, ("notes"         , "<i>Remarks:</i> ")
	, ("complexity"    , "<i>Complexity:</i> ")
	, ("postconditions", "<i>Postconditions:</i> ")
	, ("postcondition" , "<i>Postcondition:</i> ")
	, ("realnotes"     , "<i>Notes:</i> ")
	, ("sync"          , "<i>Synchronization:</i> ")
	, ("errors"        , "<i>Error conditions:</i> ")
	, ("xref"          , "See also:")
	, ("seebelow"      , "see below")
	, ("unspec"        , "<i>unspecified</i>")
	, ("Cpp"           , "C++")
	, ("sum"           , "∑")
	, ("ntmbs"         , "<span style='font-variant:small-caps'>ntmbs</span>")
	, ("ntbs"          , "<span style='font-variant:small-caps'>ntbs</span>")
	, ("shr"           , ">>")
	, ("cv"            , "cv")
	, ("shl"           , "&lt;&lt;")
	, ("br"            , "<br/>&emsp;")
	, ("sim"           , "~")
	, ("quad"          , "&emsp;&ensp;")
	, ("uniquens"      , "<i>unique</i>") -- they use weird squiqqly letters
	, ("unun"          , "__")
	, ("^"             , "^")
	, ("ldots"         , "&hellip;")
	, ("times"         , "&times;")
	, ("&"             , "&amp;")
	, ("textbackslash" , "\\")
	, ("colcol"        , "::")
	, ("tilde"         , "~")
	, ("hspace"        , " ")
	, ("equiv"         , "&equiv;")
	, ("opt"           , "<sub><small>opt</small></sub>")
	, ("expos"         , "<i>exposition-only</i>")
	]

makeSpan, makeDiv :: [String]
makeSpan = words "ncbnf bnf indented ncsimplebnf ttfamily itemdescr minipage"
makeDiv = words "defn definition cvqual tcode textit textnormal term emph grammarterm exitnote footnote terminal nonterminal mathit enternote exitnote enterexample exitexample ncsimplebnf ncbnf bnf indented paras ttfamily"


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
	render (TeXSeq x y               ) = render x ++ render y
	render (TeXRaw x                 ) = Text.replace "~" " "
	                                   $ Text.replace ">" "&gt;"
	                                   $ Text.replace "<" "&lt;"
	                                   $ Text.replace "&" "&amp;"
	                                   $ x
	render (TeXComment _             ) = ""
	render (TeXLineBreak _ _         ) = "<br/>"
	render (TeXEmpty                 ) = ""
	render (TeXBraces t              ) = "{" ++ render t ++ "}"
	render (TeXMath Dollar t         ) = render t
	render (TeXComm "ref" [FixArg (TeXRaw x)]) = xml "a" [("href", "../" ++ x)] ("[" ++ x ++ "]")
	render (TeXComm "impldef" _) = "implementation-defined"
	render (TeXComm "definition" [FixArg (TeXRaw name), FixArg (TeXRaw abbr)])
		= ("<br/><br/>" ++) $ spanTag "definition" $
			xml "a" [("id", abbr), ("class", "abbr_ref")] abbr ++ "<br/>" ++ name
	render (TeXComm "xname" [FixArg (TeXRaw "far")]) = "__far"
	render (TeXComm "impdefx" [FixArg _description_for_index]) = "implementation-defined"
	render (TeXComm "mname" [FixArg (TeXRaw s)]) = mconcat ["__", s, "__"]
	render (TeXComm "nontermdef" [FixArg (TeXRaw s)]) = mconcat [s, ":"]
	render (TeXComm "bigoh" [FixArg (TeXRaw s)]) = "O(" ++ s ++ ")"
	render (TeXComm "defnx" [FixArg a, FixArg _description_for_index]) = render a
	render (TeXComm "range" [FixArg (TeXRaw x), FixArg (TeXRaw y)]) = mconcat ["[", x, ", ", y, ")"]
	render (TeXComm "crange" [FixArg (TeXRaw x), FixArg (TeXRaw y)]) = mconcat ["[", x, ", ", y, "]"]
	render (TeXComm x s)
	    | x `elem` kill                = ""
	    | null s, Just y <-
	       lookup x simpleMacros       = y
	    | otherwise                    = spanTag (Text.pack x) (render (map texFromArg s))
	render (TeXCommS s               )
	    | s `elem` literal             = Text.pack s
	    | Just x <-
	       lookup s simpleMacros       = x
	    | s `elem` kill                = ""
	    | otherwise                    = spanTag (Text.pack s) ""
	render (TeXEnv "codeblock" [] t)   = spanTag "codeblock" $ Text.replace "@" "" $ render t
	render (TeXEnv "itemdecl" [] t)    = spanTag "itemdecl" $ Text.replace "@" "" $ render t
	render (TeXEnv e u t)
	    | e `elem` makeSpan            = spanTag (Text.pack e) (render t)
	    | e `elem` makeDiv, null u     = xml "div" [("class", Text.pack e)] (render t)
	    | otherwise                    = spanTag "poo" ("[" ++ Text.pack e ++ "]")
	render x                           = error $ show x

instance Render Element where
	render (LatexElements t) = xml "p" [] $ render t
	render (Itemized ps) = xml "ul" [] $ mconcat $ map (xml "li" [] . render) ps
	render (Enumerated ps) = xml "ol" [] $ mconcat $ map (xml "li" [] . render) ps

renderParagraph :: Text -> (Int, Paragraph) -> Text
renderParagraph idPrefix (show -> Text.pack -> i, x) =
	xml "div" [("class", "para"), ("id", idPrefix ++ i)] $
	render (anchor{aClass="paranumber", aHref="#" ++ idPrefix ++ i,aText=i}) ++
	render x

linkToSection :: Text -> Text -> Anchor
linkToSection hrefPrefix abbr = anchor{
	aClass = "abbr_ref",
	aHref  = hrefPrefix ++ abbr,
	aText  = "[" ++ abbr ++ "]"}

type SecNum = (Either Int Char, [Int])

numberSubsecs :: SecNum -> [Section] -> [(SecNum, Section)]
numberSubsecs (e, x) = zip [(e, x ++ [i]) | i <- [1..]]

secNumLength :: SecNum -> Int
secNumLength (_, x) = 1 + length x

renderSection :: Maybe Text -> Bool -> (SecNum, Section) -> (Text, Bool)
renderSection specific parasEmitted (secNum, Section{..})
	| full = (, True) $
		xml "div" [("id", abbreviation)] $ header ++
		xml "p" [] (render preamble) ++
		mconcat (map
			(renderParagraph (if parasEmitted then abbreviation ++ "-" else ""))
			(zip [1..] paragraphs)) ++
		mconcat (fst . renderSection Nothing True . numberSubsecs secNum subsections)
	| not anysubcontent = ("", False)
	| otherwise =
		( header ++
		  mconcat (fst . renderSection specific False . numberSubsecs secNum subsections)
		, anysubcontent )
	where
		hrefPrefix = if specific == Just abbreviation then "../#" else "../"
		full = specific == Nothing || specific == Just abbreviation
		header = h Nothing (secNumLength secNum) $
			(if full
				then render $ anchor{
					aClass = "secnum",
					aHref  = "#" ++ abbreviation,
					aText  = secNumText secNum }
				else spanTag "secnum" (secNumText secNum))
			++ render sectionName
			++ render (linkToSection hrefPrefix abbreviation)
		anysubcontent =
			or $ map (snd . renderSection specific True)
			   $ numberSubsecs secNum subsections

secNumText :: SecNum -> Text
secNumText (e, x) =
	Text.intercalate "." (
		Text.pack (either show (:[]) e) :
		Text.pack . show . x)

abbreviations :: Section -> [Text]
abbreviations Section{..} = abbreviation : concatMap abbreviations subsections

chapterNums :: [SecNum]
chapterNums = (, []) . ((Left . [1..26]) ++ (Right . ['A'..])) -- todo

sectionFileContent :: [Section] -> Text -> Text
sectionFileContent chapters abbreviation =
	fileContent
		("[" ++ abbreviation ++ "]")
		(mconcat (fst . map (renderSection (Just abbreviation) False) (zip chapterNums chapters)))
		".."

tocFileContent :: [Section] -> Text
tocFileContent chapters =
		fileContent
			"14882: Contents"
			(mconcat (map section (zip chapterNums chapters)))
			"."
	where
		section :: (SecNum, Section) -> Text
		section (secNum, Section{..}) =
			xml "div" [("id", abbreviation)] $
			h Nothing (secNumLength secNum) (
				xml "small" [] (
				spanTag "secnum" (secNumText secNum) ++
				render (sectionName, linkToSection "" abbreviation))) ++
			mconcat (map section (numberSubsecs secNum subsections))

fileContent :: Text -> Text -> Text -> Text
fileContent title body pathHome =
	"<!DOCTYPE html>" ++
	"<html>" ++
		"<head>" ++
			"<title>" ++ title ++ "</title>" ++
			"<meta charset='UTF-8'/>" ++
			"<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "/14882.css'/>" ++
		"</head>" ++
		"<body>" ++ body ++ "</body>" ++
	"</html>"

readStuff :: IO [Section]
readStuff = do
	putStr "Reading... "; hFlush stdout
	chapters <- load14882
	putStrLn $ show (length chapters) ++ " chapters"
	return chapters

writeStuff :: [Section] -> IO ()
writeStuff chapters = do
	let outputDir = "14882"
	putStr $ "Writing to " ++ outputDir ++ "/ "; hFlush stdout

	createDirectoryIfMissing True outputDir

	copyFile "14882.css" (outputDir ++ "/14882.css")

	TextIO.writeFile (outputDir ++ "/index.html") $ tocFileContent chapters

	let allAbbrs = concatMap abbreviations chapters
	forM_ allAbbrs $ \abbreviation -> do
		putStr "."; hFlush stdout
		let dir = outputDir ++ "/" ++ Text.unpack abbreviation
		createDirectoryIfMissing True dir
		TextIO.writeFile (dir ++ "/index.html") $ sectionFileContent chapters abbreviation
	putStrLn $ " " ++ show (length allAbbrs) ++ " sections"

main :: IO ()
main = readStuff >>= writeStuff
