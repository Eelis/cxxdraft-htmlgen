{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns, NamedFieldPuns #-}

module SectionPages
	( writeSectionFiles
	, writeSingleSectionFile
	, writeFiguresFile
	, writeFigureFiles
	, writeTablesFile
	, writeTableFiles
	, writeIndexFiles
	, writeFootnotesFile
	, writeCssFile
	, writeXrefDeltaFiles
	) where

import Prelude hiding ((++), (.), writeFile)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (when, forM_)
import Control.Arrow (first)
import Data.Maybe (fromJust)
import System.Process (readProcess)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as TextBuilder
import LaTeXBase (LaTeXUnit(..))
import Pages (writePage, pageContent, pagePath, PageStyle(..), fileContent, outputDir, Link(..))
import Render (render, concatRender, simpleRender2, renderFig, abbrHref,
	defaultRenderContext, renderTab, RenderContext(..), Page(..),linkToSection, squareAbbr,
	secnum, renderLatexParas, isSectionPage, parentLink, renderIndex)
import Document
import Util (urlChars, (++), (.), h, anchor, xml, Anchor(..), Text, writeFile, intercalateBuilders)

renderParagraph :: RenderContext -> TextBuilder.Builder
renderParagraph ctx@RenderContext{nearestEnclosing=Left Paragraph{..}, draft=Draft{..}} =
		(case paraNumber of
			Just i -> renderNumbered (Text.pack $ show i)
			Nothing -> id)
		$ (if paraInItemdescr then xml "div" [("class", "itemdescr")] else id)
		$ (sourceLink
		  ++ renderLatexParas paraElems ctx'{extraIndentation=if paraInItemdescr then 12 else 0})
			-- the 12 here must match div.itemdescr's margin-left value in mm
	where
		urlBase = Text.replace "/commit/" "/tree/" commitUrl ++ "/source/"
		sourceLink :: TextBuilder.Builder
		sourceLink
			| Just SourceLocation{..} <- paraSourceLoc =
				xml "div" [("class", "sourceLinkParent")]
				$ simpleRender2 $ anchor
					{ aClass = "sourceLink"
					, aText = "#"
					, aHref = urlBase ++ Text.pack (sourceFile ++ "#L" ++ show sourceLine) }
			| otherwise = ""

		renderNumbered :: Text -> TextBuilder.Builder -> TextBuilder.Builder
		renderNumbered n =
			let
				idTag = if isSectionPage (page ctx) then [("id", mconcat (idPrefixes ctx) ++ n)] else []
				a = anchor
					{ aClass = "marginalized"
					, aHref  =
						if isSectionPage (page ctx)
							then "#" ++ urlChars (mconcat (idPrefixes ctx)) ++ n
							else "SectionToSection/" ++ urlChars (abbreviation paraSection) ++ "#" ++ n
					, aText  = TextBuilder.fromText n }
				classes = "para" ++
					(if all (not . normative) (paraElems >>= sentences >>= sentenceElems)
						then " nonNormativeOnly"
						else "")
			in
				xml "div" (("class", classes) : idTag) .
				(xml "div" [("class", "marginalizedparent")] (render a ctx') ++)
		ctx' = case paraNumber of
			Just n -> ctx{ idPrefixes = idPrefixes ctx ++ [Text.pack (show n) ++ "."] }
			Nothing -> ctx
renderParagraph _ = undefined

tocSection :: RenderContext -> Section -> TextBuilder.Builder
tocSection _ Section{sectionKind=DefinitionSection _} = ""
tocSection ctx s@Section{..} = header ++ mconcat (tocSection ctx . subsections)
  where
    header = h (min 4 $ 1 + length parents) $
        secnum 0 "" s ++ " "
        ++ render ( sectionName ++ [TeXRaw " "]
                  , anchor{ aHref = abbrHref abbreviation ctx, aText = squareAbbr True abbreviation, aClass="abbr_ref" })
                  ctx{ inSectionTitle = True }
        ++ "<div style='clear:right'></div>"

renderSection :: RenderContext -> Maybe Section -> Bool -> Section -> (TextBuilder.Builder, Bool)
renderSection context specific parasEmitted s@Section{abbreviation, subsections, sectionFootnotes, paragraphs}
	| full = (, True) $
		idDiv header ++
		(if specific == Just s && any (not . isDefinitionSection . sectionKind) subsections then toc else "") ++
		mconcat (map
			(\p -> renderParagraph (context{nearestEnclosing=Left p,idPrefixes=if parasEmitted then [secOnPage ++ "-"] else []}))
			paragraphs) ++
		(if null sectionFootnotes then "" else "<div class='footnoteSeparator'></div>") ++
		concatRender sectionFootnotes context{nearestEnclosing=Right s} ++
		mconcat (fst . renderSection context Nothing True . subsections)
	| not anysubcontent = ("", False)
	| otherwise =
		( header ++
		  mconcat (fst . renderSection context specific False . subsections)
		, anysubcontent )
	where
		idDiv
			| specific == Just s = id
			| otherwise = xml "div" [("id", secOnPage), ("class", "section")]
		secOnPage :: Text
		secOnPage = case page context of
			SectionPage parent -> parentLink parent abbreviation
			_ -> abbreviation
		full = specific == Nothing || specific == Just s
		reduceHeaderIndent = case page context of
		    SectionPage p | specific == Nothing -> length (parents p) + 1
		    _ -> 0
		header = sectionHeader reduceHeaderIndent (min 4 $ 1 + length (parents s)) s
			(if specific == Nothing && isSectionPage (page context) then "#" ++ urlChars secOnPage else "")
			abbr context
		toc = "<hr>" ++ mconcat (tocSection context . subsections) ++ "<hr>"
		abbr
			| specific == Just s && not (null (parents s))
				= anchor
			| Just sp <- specific, sp /= s, not (null (parents s))
				= anchor{aHref = "SectionToSection/" ++ urlChars abbreviation ++ "#" ++ parentLink s (Document.abbreviation sp)}
			| otherwise = linkToSection
					(if null (parents s) then SectionToToc else SectionToSection)
					abbreviation
		anysubcontent =
			or $ map (snd . renderSection context specific True)
			   $ subsections

sectionFileContent :: PageStyle -> TextBuilder.Builder -> TextBuilder.Builder -> Text
sectionFileContent sfs title body = pageContent sfs $ fileContent pathHome title sectionPageCss body
  where
    pathHome = if sfs == InSubdir then "../" else ""
    sectionPageCss =
        "<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "expanded.css' title='Normal'>" ++
        "<link rel='alternate stylesheet' type='text/css' href='" ++ pathHome ++ "colored.css' title='Notes and examples colored'>" ++
        "<link rel='alternate stylesheet' type='text/css' href='" ++ pathHome ++ "normative-only.css' title='Notes and examples hidden'>"

writeSectionFile :: FilePath -> PageStyle -> TextBuilder.Builder -> TextBuilder.Builder -> IO ()
writeSectionFile n sfs title body = writePage n sfs (sectionFileContent sfs title body)

sectionHeader :: Int -> Int -> Section -> Text -> Anchor -> RenderContext -> TextBuilder.Builder
sectionHeader reduceIndent hLevel s@Section{..} secnumHref abbr_ref ctx
    | isDef = xml "h4" [("style", "margin-bottom:3pt")] $ num ++ abbrR ++ name
    | abbreviation == "bibliography" = h hLevel name
    | otherwise = h hLevel $ num ++ " " ++ name ++ " " ++ abbrR
  where
    num = secnum reduceIndent secnumHref s
    abbrR = simpleRender2 abbr_ref{aClass = "abbr_ref", aText = squareAbbr False abbreviation}
    name = render sectionName ctx{inSectionTitle=True}
    isDef = isDefinitionSection sectionKind

writeFiguresFile :: PageStyle -> Draft -> IO ()
writeFiguresFile sfs draft = writeSectionFile "fig" sfs "14882: Figures" $
	"<h1>Figures <a href='SectionToToc/fig' class='abbr_ref'>[fig]</a></h1>"
	++ mconcat (uncurry r . figures draft)
	where
		r :: Paragraph -> Figure -> TextBuilder.Builder
		r p f@Figure{..} =
			renderFig True f ("./SectionToSection/" ++ urlChars figureAbbr) False True ctx
			where ctx = defaultRenderContext{draft=draft, nearestEnclosing=Left p, page=FiguresPage}

writeTablesFile :: PageStyle -> Draft -> IO ()
writeTablesFile sfs draft = writeSectionFile "tab" sfs "14882: Tables" $
	"<h1>Tables <a href='SectionToToc/tab' class='abbr_ref'>[tab]</a></h1>"
	++ mconcat (uncurry r . tables draft)
	where
		r :: Paragraph -> Table -> TextBuilder.Builder
		r p t@Table{tableSection=Section{..}, ..} =
			renderTab True t ("./SectionToSection/" ++ urlChars tableAbbr) False True ctx
			where ctx = defaultRenderContext{
				draft = draft,
				nearestEnclosing = Left p,
				page = TablesPage,
				idPrefixes = [fromJust (Text.stripPrefix "tab:" tableAbbr) ++ "-"]}

writeFootnotesFile :: PageStyle -> Draft -> IO ()
writeFootnotesFile sfs draft = writeSectionFile "footnotes" sfs "14882: Footnotes" $
	"<h1>List of Footnotes</h1>"
	++ mconcat (uncurry r . footnotes draft)
	where
		r :: Section -> Footnote -> TextBuilder.Builder
		r s fn = render fn defaultRenderContext{draft=draft, nearestEnclosing = Right s, page=FootnotesPage}

writeSingleSectionFile :: PageStyle -> Draft -> String -> IO ()
writeSingleSectionFile sfs draft abbr = do
	let Just section@Section{..} = Document.sectionByAbbr draft (Text.pack abbr)
	let baseFilename = Text.unpack abbreviation
	writeSectionFile baseFilename sfs (squareAbbr False abbreviation) $ mconcat $ fst . renderSection (defaultRenderContext{draft=draft,page=SectionPage section}) (Just section) False . chapters draft
	putStrLn $ "  " ++ baseFilename

writeTableFiles :: PageStyle -> Draft -> IO ()
writeTableFiles sfs draft =
	forM_ (snd . tables draft) $ \tab@Table{..} -> do
		let
			context = defaultRenderContext{draft=draft, page=TablePage tab, nearestEnclosing=Right tableSection}
			header :: Section -> TextBuilder.Builder
			header sec = sectionHeader 0 (min 4 $ 1 + length (parents sec)) sec "" anchor{aHref=href} context
				where href="SectionToSection/" ++ urlChars (abbreviation sec) ++ "#" ++ urlChars tableAbbr
			headers = mconcat $ map header $ reverse $ tableSection : parents tableSection
		writeSectionFile (Text.unpack tableAbbr) sfs (TextBuilder.fromText $ "[" ++ tableAbbr ++ "]") $
			headers ++ renderTab True tab "" True False context

writeFigureFiles :: PageStyle -> Draft -> IO ()
writeFigureFiles sfs draft =
	forM_ (snd . figures draft) $ \fig@Figure{..} -> do
		let
			context = defaultRenderContext{draft=draft, page=FigurePage fig, nearestEnclosing=Right figureSection}
			header :: Section -> TextBuilder.Builder
			header sec = sectionHeader 0 (min 4 $ 1 + length (parents sec)) sec "" anchor{aHref=href} context
				where href="SectionToSection/" ++ urlChars (abbreviation sec) ++ "#" ++ urlChars figureAbbr
			headers = mconcat $ map header $ reverse $ figureSection : parents figureSection
		writeSectionFile (Text.unpack figureAbbr) sfs (TextBuilder.fromText $ "[" ++ figureAbbr ++ "]") $
			headers ++ renderFig True fig "" True False context

writeSectionFiles :: PageStyle -> Draft -> [IO ()]
writeSectionFiles sfs draft = flip map (zip names contents) $ \(n, content) -> do
		when (sfs == InSubdir) $ createDirectoryIfMissing True (outputDir ++ n)
		writeFile (pagePath n sfs) content
	where
		secs = Document.sections draft
		renSec section@Section{..} = (Text.unpack abbreviation, sectionFileContent sfs title body)
		  where
			title = squareAbbr False abbreviation
			body = mconcat $ fst . renderSection (defaultRenderContext{draft=draft,page=SectionPage section}) (Just section) False . chapters draft
		fullbody = mconcat $ fst . renderSection defaultRenderContext{draft=draft, page=FullPage} Nothing True . chapters draft
		fullfile = ("full", sectionFileContent sfs "14882" fullbody)
		files = fullfile : map renSec secs
		names = fst . files
		contents = snd . files

writeIndexFile :: PageStyle -> Draft -> String -> IndexTree -> IO ()
writeIndexFile sfs draft cat index =
	writeSectionFile cat sfs ("14882: " ++ indexCatName cat) $
		h 1 (indexCatName cat) ++ renderIndex defaultRenderContext{page=IndexPage (Text.pack cat), draft=draft} index

writeIndexFiles :: PageStyle -> Draft -> Index -> [IO ()]
writeIndexFiles sfs draft index = flip map (Map.toList index) $ uncurry (writeIndexFile sfs draft) . first Text.unpack

writeCssFile :: IO ()
writeCssFile = do
	base <- Text.pack . readFile "14882.css"
	let
		replaceFonts =
			Text.replace
				".MJXc-TeX-sans-R {font-family: MJXc-TeX-sans-R,MJXc-TeX-sans-Rw}"
				".MJXc-TeX-sans-R {font-family: 'Noto Sans'; font-size: 10pt; }" .
			Text.replace
				".MJXc-TeX-type-R {font-family: MJXc-TeX-type-R,MJXc-TeX-type-Rw}"
				".MJXc-TeX-type-R {font-family: 'Noto Sans Mono'; font-size: 10pt; }" .
			Text.replace
				".MJXc-TeX-main-R {font-family: MJXc-TeX-main-R,MJXc-TeX-main-Rw}"
				".MJXc-TeX-main-R {}" .
			Text.replace
				".MJXc-TeX-math-I {font-family: MJXc-TeX-math-I,MJXc-TeX-math-Ix,MJXc-TeX-math-Iw}"
				".MJXc-TeX-math-I {font-style: italic}" .
			Text.replace
				".MJXc-TeX-main-I {font-family: MJXc-TeX-main-I,MJXc-TeX-main-Ix,MJXc-TeX-main-Iw}"
				".MJXc-TeX-main-I {font-style: italic}"
		-- Replace fonts to make sure code in formulas matches code in code blocks, etc.
	mjx <- Text.replace "display: block" "display: block;background:inherit" . replaceFonts . Text.pack .
		readProcess "tex2html" ["--css", ""] ""
	writeFile (outputDir ++ "/14882.css") (base ++ mjx)

writeXrefDeltaFiles :: PageStyle -> Draft -> [IO ()]
writeXrefDeltaFiles sfs draft = flip map (xrefDelta draft) $ \(from, to) ->
	writeSectionFile (Text.unpack from) sfs (squareAbbr False from) $
		if to == []
			then "Subclause " ++ squareAbbr False from ++ " was removed."
			else "See " ++ intercalateBuilders ", " (flip render ctx . to) ++ "."
	where ctx = defaultRenderContext{draft=draft, page=XrefDeltaPage}
