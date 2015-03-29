{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	TupleSections,
	ViewPatterns #-}

import Load14882 (Element(..), Paragraph, ChapterKind(..), Section(..), Chapter, load14882)

import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), MathType(..))
import Data.Text (Text)
import Data.Char (isSpace)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text.IO (writeFile)
import Data.Monoid (Monoid(mappend), mconcat)
import Data.Char (isAlphaNum)
import Control.Monad (forM_)
import qualified Prelude
import Prelude hiding (take, last, (.), (++), writeFile)
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

rangeElem :: Char -> Text -> Text -> Char -> Text
rangeElem open from to close
	= spanTag "range" $ Text.pack [open] ++ from ++ ", " ++ to ++ Text.pack [close]

h :: Maybe Text -> Int -> Text -> Text
h mc = flip xml (maybe [] ((:[]) . ("class",)) mc) . ("h" ++) . Text.pack . show

kill, literal :: [String]
kill = ["indextext", "indexdefn", "indexlibrary", "indeximpldef", "printindex", "clearpage", "renewcommand", "brk", "newcommand", "footnotetext", "enlargethispage", "index", "noindent", "indent", "vfill", "pagebreak", "topline", "xspace", "!", "linebreak"]
literal = [" ", "cv", "#", "{", "}", "-", "~", "%", ",", ""]

texFromArg :: TeXArg -> LaTeX
texFromArg (FixArg t) = t
texFromArg (OptArg t) = t
texFromArg (SymArg t) = t
texFromArg _ = error "no"

ampersandMagic, removeStartMagic, removeEndMagic, lineBreakMagic, tabMagic :: Text
ampersandMagic = "\^^"  -- record separator
removeStartMagic = "\STX"
removeEndMagic = "\ETX"
lineBreakMagic = "\US"
tabMagic = "\t"

simpleMacros :: [(String, Text)]
simpleMacros =
	[ ("dcr"            , "--")
	, ("prime"          , "'")
	, ("atsign"         , "@")
	, ("copyright"      , "&copy;")
	, ("textregistered" , "&reg;")
	, ("change"         , "<br/><b>Change:</b> ")
	, ("rationale"      , "<br/><b>Rationale:</b> ")
	, ("difficulty"     , "<br/><b>Difficulty of converting:</b> ")
	, ("howwide"        , "<br/><b>How widely used:</b> ")
	, ("effect"         , "<br/><b>Effect on original feature:</b> ")
	, ("required"       , "<span class=\"funcdesc\">Required behavior</span>")
	, ("requires"       , "<span class=\"funcdesc\">Requires</span>")
	, ("effects"        , "<span class=\"funcdesc\">Effects</span>")
	, ("postconditions" , "<span class=\"funcdesc\">Postconditions</span>")
	, ("postcondition"  , "<span class=\"funcdesc\">Postcondition</span>")
	, ("preconditions"  , "<span class=\"funcdesc\">Requires</span>")
	, ("precondition"   , "<span class=\"funcdesc\">Requires</span>")
	, ("returns"        , "<span class=\"funcdesc\">Returns</span>")
	, ("throws"         , "<span class=\"funcdesc\">Throws</span>")
	, ("default"        , "<span class=\"funcdesc\">Default behavior</span>")
	, ("complexity"     , "<span class=\"funcdesc\">Complexity</span>")
	, ("remark"         , "<span class=\"funcdesc\">Remark</span>")
	, ("remarks"        , "<span class=\"funcdesc\">Remarks</span>")
	, ("note"           , "<span class=\"funcdesc\">Remark</span>")
	, ("notes"          , "<span class=\"funcdesc\">Remarks</span>")
	, ("realnote"       , "<span class=\"funcdesc\">Note</span>")
	, ("realnotes"      , "<span class=\"funcdesc\">Notes</span>")
	, ("errors"         , "<span class=\"funcdesc\">Error conditions</span>")
	, ("sync"           , "<span class=\"funcdesc\">Synchronization</span>")
	, ("implimits"      , "<span class=\"funcdesc\">Implementation limits</span>")
	, ("replaceable"    , "<span class=\"funcdesc\">Replaceable</span>")
	, ("exceptionsafety", "<span class=\"funcdesc\">Exception safety</span>")
	, ("returntype"     , "<span class=\"funcdesc\">Return type</span>")
	, ("cvalue"         , "<span class=\"funcdesc\">Value</span>")
	, ("ctype"          , "<span class=\"funcdesc\">Type</span>")
	, ("ctypes"         , "<span class=\"funcdesc\">Types</span>")
	, ("dtype"          , "<span class=\"funcdesc\">Default type</span>")
	, ("ctemplate"      , "<span class=\"funcdesc\">Class template</span>")
	, ("templalias"     , "<span class=\"funcdesc\">Alias template</span>")
	, ("xref"           , "See also:")
	, ("seebelow"       , "see below")
	, ("unspec"         , "<i>unspecified</i>")
	, ("Cpp"            , "C++")
	, ("CppIII"         , "C++ 2003")
	, ("CppXI"          , "C++ 2011")
	, ("CppXIV"         , "C++ 2014")
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
	, ("xsee"           , "<span class=\"textsc\">See:</span> ")
	, ("ntmbs"          , "<span class=\"textsc\">ntmbs</span>")
	, ("ntbs"           , "<span class=\"textsc\">ntbs</span>")
	, ("shr"            , ">>")
	, ("cv"             , "cv")
	, ("shl"            , "&lt;&lt;")
	, ("br"             , "<br/>&emsp;")
	, ("sim"            , "~")
	, ("quad"           , "&emsp;&ensp;")
	, ("uniquens"       , "<i>unique</i>") -- they use weird squiqqly letters
	, ("impdef"         , "<i>implementation-defined</i>")
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
	, (";"              , " ")
	, ("min"            , "<span class=\"mathrm\">min</span>")
	, ("max"            , "<span class=\"mathrm\">max</span>")
	, ("bmod"           , "<span class=\"mathrm\">mod</span>")
	, ("exp"            , "<span class=\"mathrm\">exp</span>")
	, ("ln"             , "<span class=\"mathrm\">ln</span>")
	, ("log"            , "<span class=\"mathrm\">log</span>")
	, ("opt"            , "<sub><small>opt</small></sub>")
	, ("expos"          , "<i>exposition-only</i>")
	, ("macro"          , "<span class=\"centry\">Macro:</span>")
	, ("macros"         , "<span class=\"centry\">Macros:</span>")
	, ("function"       , "<span class=\"centry\">Function:</span>")
	, ("functions"      , "<span class=\"centry\">Functions:</span>")
	, ("mfunctions"     , "<span class=\"centry\">Math Functions:</span>")
	, ("cfunctions"     , "<span class=\"centry\">Classification/comparison Functions:</span>")
	, ("type"           , "<span class=\"centry\">Type:</span>")
	, ("types"          , "<span class=\"centry\">Types:</span>")
	, ("values"         , "<span class=\"centry\">Values:</span>")
	, ("struct"         , "<span class=\"centry\">Struct:</span>")
	, ("endfirsthead"   , removeStartMagic)
	, ("endhead"        , removeEndMagic)
	, ("bitand"         , "<span class=\"mathsf\">bitand</span>")
	, ("bitor"          , "<span class=\"mathsf\">bitor</span>")
	, ("xor"            , "<span class=\"mathsf\">xor</span>")
	, ("rightshift"     , "<span class=\"mathsf\">rshift</span>")
	]

makeSpan, makeDiv, makeBnfTable, makeBnfPre, makeTh, makeRowsep, makeCodeblock :: [String]
makeSpan = words "ncbnf indented ncsimplebnf ttfamily itemdescr minipage"
makeDiv = words "defn definition cvqual tcode textit textnormal term emph grammarterm exitnote footnote terminal nonterminal mathit enternote exitnote enterexample exitexample ncsimplebnf ncbnf indented paras ttfamily"
makeBnfTable = words "bnfkeywordtab bnftab"
makeBnfPre = words "bnf"
makeTh = words "lhdr rhdr chdr"
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
	render (TeXSeq x y               ) = render x ++ render y
	render (TeXRaw x                 ) = Text.replace "~" " "
	                                   $ Text.replace "--" "–"
	                                   $ Text.replace "---" "—"
	                                   $ Text.replace ">" "&gt;"
	                                   $ Text.replace "<" "&lt;"
	                                   $ Text.replace "&" ampersandMagic
	                                   $ x
	render (TeXComment _             ) = ""
	render (TeXCommS "br"            ) = "<br/>"
	render (TeXLineBreak _ _         ) = "<br/>"
	render (TeXEmpty                 ) = ""
	render (TeXBraces t              ) = "{" ++ render t ++ "}"
	render (TeXMath _ t              ) = spanTag "math" $ render t
	render (TeXComm "ref" [FixArg x])  = render $ linkToSection "" SectionToSection x
	render (TeXComm "impldef" _) = "implementation-defined"
	render (TeXComm "impdefx" [FixArg _description_for_index]) = "implementation-defined"
	render (TeXComm "xname" [FixArg (TeXRaw s)]) = spanTag "texttt" $ "_<span class=\"ungap\"></span>_" ++ s
	render (TeXComm "mname" [FixArg (TeXRaw s)]) = spanTag "texttt" $ "_<span class=\"ungap\"></span>_" ++ s ++ "_<span class=\"ungap\"></span>_"
	render (TeXComm "nontermdef" [FixArg (TeXRaw s)]) = mconcat [spanTag "nontermdef" s, ":"]
	render (TeXComm "bigoh" [FixArg content]) = spanTag "math" $ mconcat [spanTag "mathscr" "𝓞", "(", render content, ")"]
	render (TeXComm "defnx" [FixArg a, FixArg _description_for_index]) = render a
	render (TeXComm "range" [FixArg (TeXRaw x), FixArg (TeXRaw y)]) = rangeElem '[' x y ')'
	render (TeXComm "orange" [FixArg (TeXRaw x), FixArg (TeXRaw y)]) = rangeElem '(' x y ')'
	render (TeXComm "crange" [FixArg (TeXRaw x), FixArg (TeXRaw y)]) = rangeElem '[' x y ']'
	render (TeXComm "brange" [FixArg (TeXRaw x), FixArg (TeXRaw y)]) = rangeElem '(' x y ']'
	render (TeXComm "multicolumn" [FixArg (TeXRaw n), _, FixArg content]) = xml "td" [("colspan", n)] $ render content
	render (TeXComm "leftshift" [FixArg content]) = mconcat [spanTag "mathsf" "lshift", xml "sub" [("class", "math")] $ render content]
	render (TeXComm "state" [FixArg a, FixArg b]) = mconcat [spanTag "tcode" (render a), xml "sub" [("class", "math")] $ render b]
	render (TeXComm x s)
	    | x `elem` kill                = ""
	    | x `elem` makeTh              = case s of [FixArg y] -> xml "th" [] $ render y
	    | null s, Just y <-
	       lookup x simpleMacros       = y
	    | [FixArg z] <- s, Just y <-
	       lookup x simpleMacros       = mconcat [y, render z]
	    | otherwise                    = spanTag (Text.pack x) (render (map texFromArg s))
	render (TeXCommS s               )
	    | s `elem` literal             = Text.pack s
	    | Just x <-
	       lookup s simpleMacros       = x
	    | s `elem` makeRowsep          = "</td></tr><tr class=\"rowsep\"><td>"
	    | s `elem` kill                = ""
	    | otherwise                    = spanTag (Text.pack s) ""
	render (TeXEnv "itemdecl" [] t)    = spanTag "itemdecl" $ Text.replace "@" "" $ Text.replace ampersandMagic "&amp;" $ render t
	render (TeXEnv e u t)
	    | e `elem` makeCodeblock       = spanTag "codeblock" $ Text.replace "@" "" $ Text.replace ampersandMagic "&amp;" $ render t
	    | e `elem` makeSpan            = spanTag (Text.pack e) (render t)
	    | e `elem` makeDiv, null u     = xml "div" [("class", Text.pack e)] (render t)
	    | otherwise                    = spanTag "poo" ("[" ++ Text.pack e ++ "]")

instance Render Element where
	render (LatexElements t) = xml "p" [] $ render t
	render (Bnf e t)
		| e `elem` makeBnfTable = renderBnfTable t
		| e `elem` makeBnfPre = bnf t
	render (Table e u t) = renderTable e u [] $ render $ preprocessTable t
	render (Enumerated ek ps) = t $ mconcat $ map (xml "li" [] . render) ps
		where
			t = case ek of
				"enumeraten" -> xml "ol" []
				"enumeratea" -> xml "ol" []
				"enumerate" -> xml "ol" []
				"itemize" -> xml "ul" []
				"description" -> xml "ul" []
				_ -> undefined

-- Explicit <br/>'s are redundant in <pre>, so strip them.
preprocessPre :: LaTeX -> LaTeX
preprocessPre (TeXCommS "br") = TeXEmpty
preprocessPre (TeXEnv e a c) = TeXEnv e a (preprocessPre c)
preprocessPre (TeXSeq a b) = TeXSeq (preprocessPre a) (preprocessPre b)
preprocessPre rest = rest

-- Tables need to handle line breaks specially.
preprocessTable :: LaTeX -> LaTeX
preprocessTable (TeXCommS "br") = TeXRaw lineBreakMagic
preprocessTable (TeXComm c a) = TeXComm c (map preprocessArg a)
preprocessTable (TeXEnv e a c) = TeXEnv e a (preprocessPre c)
preprocessTable (TeXSeq a b) = TeXSeq (preprocessTable a) (preprocessTable b)
preprocessTable rest = rest

preprocessArg (FixArg e) = FixArg (preprocessTable e)
preprocessArg rest = rest

bnf :: LaTeX -> Text
bnf = xml "pre" [("class", "bnf")] . render . preprocessPre

renderBnfTable :: LaTeX -> Text
renderBnfTable =
	xml "pre" [("class", "bnf")] . processHTML . render . preprocessTeX . preprocessPre
	where
		processHTML = Text.replace tabMagic "&#9;" .  Text.replace lineBreakMagic "<br/>"

		initialTab (Text.stripPrefix ">" -> Just rest) =
			tabMagic ++ rest
		initialTab other = other

		preprocessTeX (TeXBraces t) = t
		preprocessTeX (TeXSeq (TeXCommS "") (TeXSeq (TeXRaw s) rest)) =
			TeXSeq (TeXCommS "") (TeXSeq (TeXRaw $ initialTab s) $ preprocessTeX rest)
		preprocessTeX (TeXSeq (TeXCommS "") (TeXRaw s)) =
			TeXSeq (TeXCommS "") (TeXRaw $ initialTab s)
		preprocessTeX (TeXSeq a b) = TeXSeq (preprocessTeX a) (preprocessTeX b)
		preprocessTeX (TeXEnv e a c) = TeXEnv e a (preprocessTeX c)
		preprocessTeX other = other

renderTable :: String -> [TeXArg] -> [(Text, Text)] -> Text -> Text
renderTable e u attrs t =
	tableDiv e u attrs $ cleanupTable $ tableHeader e u ++ postprocessTable t

tableDiv :: String -> [TeXArg] -> [(Text, Text)] -> Text -> Text
tableDiv command args attrs content =
	xml "div" [("class", "table")] $
	xml "a" [("id", xref)] "" ++ spanTag "tabletitle" title ++ xml "table" attrs content
	where
		(title, xref) = extract command args
		extract (List.stripPrefix "libsyntab" -> Just _) [(FixArg title), (FixArg (TeXRaw xref))] =
			("Header " ++ (spanTag "tcode" $ "&lt;" ++ render title ++ "&gt;") ++ " synopsis", xref)
		extract _ ((FixArg title) : (FixArg (TeXRaw xref)) : _) = (render title, xref)
		extract _ _ = (Text.empty, Text.empty)

removeConsecWhitespace :: Text -> Text
removeConsecWhitespace = Text.unwords . filter (not . Text.null) . Text.split isSpace

cleanupTable :: Text -> Text
cleanupTable = 
	Text.replace "<td > <td " "<td " .
	Text.replace "</td> </td>" "</td>" .
	Text.replace "<tr ><td > </td></tr>" "" .
	Text.replace "<tr ><td ></td></tr>" "" .
	Text.replace "<tr ></tr>" "" .
 	Text.replace "<td > <th >" "<th>" .
 	Text.replace "</th> </td>" "</th>" .
 	removeConsecWhitespace

removeDeadContent :: Text -> Text
removeDeadContent t =
	case Text.breakOn removeStartMagic t of
		(begin, end) | not $ Text.null end -> begin ++ (removeDeadContent $ Text.drop 1 end')
			where (_, end') = Text.breakOn removeEndMagic end
		(s, "") -> s

postprocessTable :: Text -> Text
postprocessTable =
	xml "tr" [] . xml "td" [] . 
	Text.replace lineBreakMagic "<br/>" .
	Text.replace "<br/>" "</td></tr><tr ><td >" . Text.replace ampersandMagic "</td><td >" .
	removeDeadContent

tableHeader :: String -> [TeXArg] -> Text
tableHeader (List.stripPrefix "libsyntab" -> Just suffix) _ =
	case suffix of
		(List.stripPrefix "add" -> Just n) -> hdr n
		(List.stripPrefix "f" -> Just n) -> hdr n
		_ -> hdr suffix
	where
		hdr n =
			xml "tr" [] (xml "th" [] "Type" ++ xml "th" [("colspan", Text.pack . show $ read n - 1)] "Name(s)")

tableHeader "libsumtabbase" [_, _, (FixArg h1), (FixArg h2)] =
	xml "tr" [] (
		xml "th" [("colspan", "2")] (render h1) ++ (xml "th" [] $ render h2))

tableHeader command args =
	xml "tr" [] . Text.concat . map (xml "th" []) $ hdr command args
	where
		hdr :: String -> [TeXArg] -> [Text]
		hdr (List.stripPrefix "long" -> Just h) args = hdr h args
		hdr "libsumtab" _ = ["", "Subclause", "Header(s)"]
		hdr "libefftab" _ = ["Element", "Effect(s) if set"]
		hdr "libefftabmean" _ = ["Element", "Meaning"]
		hdr "libefftabvalue" _ = ["Element", "Value"]
		hdr "liberrtab" _ = ["Value", "Error condition"]
		hdr "LibEffTab" [_, _, (FixArg h), _] = ["Element", render h]
		hdr "libtab2" [_, _, _, (FixArg h1), (FixArg h2)] = [render h1, render h2]
		hdr "tokentable" [_, _, (FixArg h1), (FixArg h2)] = [render h1, render h2, render h1, render h2, render h1, render h2]
		hdr _ _ = []

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
		url (TeXCommS "firstlibchapter") = "flc" -- todo
		url (TeXCommS "lastlibchapter") = "llc" -- todo
		url x = error $ show x

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
		header = h Nothing (length sectionNums) $
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

readStuff :: IO [Chapter]
readStuff = do
	putStr "Reading... "; hFlush stdout
	chapters <- load14882
	putStrLn $ show (length chapters) ++ " chapters"
	return chapters

urlEncode :: Text -> Text
urlEncode
	= Text.replace "<" "%3c"
	. Text.replace ">" "%3e"
	. Text.replace ":" "%3a"

abbrAsPath :: LaTeX -> Text
abbrAsPath (TeXRaw x) = x
abbrAsPath (TeXSeq x y) = abbrAsPath x ++ abbrAsPath y
abbrAsPath (TeXCommS "dcr") = "--"
abbrAsPath _ = undefined

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
main = readStuff >>= writeStuff
