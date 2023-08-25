{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns #-}

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
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import Render (render, concatRender, simpleRender2, outputDir, renderFig,
	defaultRenderContext, renderTab, RenderContext(..), SectionFileStyle(..), Page(..),
	linkToSection, squareAbbr, fileContent, applySectionFileStyle,
	secnum, Link(..), renderLatexParas, isSectionPage, parentLink, renderIndex)
import Document
import Util (urlChars, (++), (.), h, anchor, xml, Anchor(..), Text, writeFile, readFile, intercalateBuilders)

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

renderSection :: RenderContext -> Maybe Section -> Bool -> Section -> (TextBuilder.Builder, Bool)
renderSection context specific parasEmitted s@Section{..}
	| full = (, True) $
		idDiv $ header ++
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
		header = sectionHeader (min 4 $ 1 + length parents) s
			(if specific == Nothing && isSectionPage (page context) then "#" ++ urlChars secOnPage else "")
			abbr context
		abbr
			| specific == Just s && not (null parents)
				= anchor
			| Just sp <- specific, sp /= s, not (null parents)
				= anchor{aHref = "SectionToSection/" ++ urlChars abbreviation ++ "#" ++ parentLink s (Document.abbreviation sp)}
			| otherwise = linkToSection
					(if null parents then SectionToToc else SectionToSection)
					abbreviation
		anysubcontent =
			or $ map (snd . renderSection context specific True)
			   $ subsections

sectionFilePath :: FilePath -> SectionFileStyle -> String
sectionFilePath n Bare = outputDir ++ n
sectionFilePath n WithExtension = outputDir ++ n ++ ".html"
sectionFilePath n InSubdir = outputDir ++ n ++ "/index.html"

sectionFileContent :: SectionFileStyle -> TextBuilder.Builder -> TextBuilder.Builder -> Text
sectionFileContent sfs title body = applySectionFileStyle sfs $ LazyText.toStrict $ TextBuilder.toLazyText $
	fileContent (if sfs == InSubdir then "../" else "") title "" body

writeSectionFile :: FilePath -> SectionFileStyle -> TextBuilder.Builder -> TextBuilder.Builder -> IO ()
writeSectionFile n sfs title body = do
	when (sfs == InSubdir) $ createDirectoryIfMissing True (outputDir ++ n)
	writeFile (sectionFilePath n sfs) (sectionFileContent sfs title body)

sectionHeader :: Int -> Section -> Text -> Anchor -> RenderContext -> TextBuilder.Builder
sectionHeader hLevel s@Section{..} secnumHref abbr_ref ctx
    | isDef = xml "h4" [("style", "margin-bottom:3pt")] $ num ++ abbrR ++ name
    | abbreviation == "bibliography" = h hLevel name
    | otherwise = h hLevel $ num ++ " " ++ name ++ " " ++ abbrR
  where
    num = secnum secnumHref s
    abbrR = simpleRender2 abbr_ref{aClass = "abbr_ref", aText = squareAbbr False abbreviation}
    name = render sectionName ctx{inSectionTitle=True}
    isDef = isDefinitionSection sectionKind

writeFiguresFile :: SectionFileStyle -> Draft -> IO ()
writeFiguresFile sfs draft = writeSectionFile "fig" sfs "14882: Figures" $
	"<h1>Figures <a href='SectionToToc/fig' class='abbr_ref'>[fig]</a></h1>"
	++ mconcat (uncurry r . figures draft)
	where
		r :: Paragraph -> Figure -> TextBuilder.Builder
		r p f@Figure{..} =
			renderFig True f ("./SectionToSection/" ++ urlChars figureAbbr) False True ctx
			where ctx = defaultRenderContext{draft=draft, nearestEnclosing=Left p, page=FiguresPage}

writeTablesFile :: SectionFileStyle -> Draft -> IO ()
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

writeFootnotesFile :: SectionFileStyle -> Draft -> IO ()
writeFootnotesFile sfs draft = writeSectionFile "footnotes" sfs "14882: Footnotes" $
	"<h1>List of Footnotes</h1>"
	++ mconcat (uncurry r . footnotes draft)
	where
		r :: Section -> Footnote -> TextBuilder.Builder
		r s fn = render fn defaultRenderContext{draft=draft, nearestEnclosing = Right s, page=FootnotesPage}

writeSingleSectionFile :: SectionFileStyle -> Draft -> String -> IO ()
writeSingleSectionFile sfs draft abbr = do
	let Just section@Section{..} = Document.sectionByAbbr draft (Text.pack abbr)
	let baseFilename = Text.unpack abbreviation
	writeSectionFile baseFilename sfs (squareAbbr False abbreviation) $ mconcat $ fst . renderSection (defaultRenderContext{draft=draft,page=SectionPage section}) (Just section) False . chapters draft
	putStrLn $ "  " ++ baseFilename

writeTableFiles :: SectionFileStyle -> Draft -> IO ()
writeTableFiles sfs draft =
	forM_ (snd . tables draft) $ \tab@Table{..} -> do
		let
			context = defaultRenderContext{draft=draft, page=TablePage tab, nearestEnclosing=Right tableSection}
			header :: Section -> TextBuilder.Builder
			header sec = sectionHeader (min 4 $ 1 + length (parents sec)) sec "" anchor{aHref=href} context
				where href="SectionToSection/" ++ urlChars (abbreviation sec) ++ "#" ++ urlChars tableAbbr
			headers = mconcat $ map header $ reverse $ tableSection : parents tableSection
		writeSectionFile (Text.unpack tableAbbr) sfs (TextBuilder.fromText $ "[" ++ tableAbbr ++ "]") $
			headers ++ renderTab True tab "" True False context

writeFigureFiles :: SectionFileStyle -> Draft -> IO ()
writeFigureFiles sfs draft =
	forM_ (snd . figures draft) $ \fig@Figure{..} -> do
		let
			context = defaultRenderContext{draft=draft, page=FigurePage fig, nearestEnclosing=Right figureSection}
			header :: Section -> TextBuilder.Builder
			header sec = sectionHeader (min 4 $ 1 + length (parents sec)) sec "" anchor{aHref=href} context
				where href="SectionToSection/" ++ urlChars (abbreviation sec) ++ "#" ++ urlChars figureAbbr
			headers = mconcat $ map header $ reverse $ figureSection : parents figureSection
		writeSectionFile (Text.unpack figureAbbr) sfs (TextBuilder.fromText $ "[" ++ figureAbbr ++ "]") $
			headers ++ renderFig True fig "" True False context

writeSectionFiles :: SectionFileStyle -> Draft -> [IO ()]
writeSectionFiles sfs draft = flip map (zip names contents) $ \(n, content) -> do
		when (sfs == InSubdir) $ createDirectoryIfMissing True (outputDir ++ n)
		writeFile (sectionFilePath n sfs) content
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

writeIndexFile :: SectionFileStyle -> Draft -> String -> IndexTree -> IO ()
writeIndexFile sfs draft cat index =
	writeSectionFile cat sfs ("14882: " ++ indexCatName cat) $
		h 1 (indexCatName cat) ++ renderIndex defaultRenderContext{page=IndexPage (Text.pack cat), draft=draft} index

writeIndexFiles :: SectionFileStyle -> Draft -> Index -> [IO ()]
writeIndexFiles sfs draft index = flip map (Map.toList index) $ uncurry (writeIndexFile sfs draft) . first Text.unpack

writeCssFile :: IO ()
writeCssFile = do
	base <- Text.pack . readFile "14882.css"
	let
		replaceFonts =
			Text.replace
				".MJXc-TeX-sans-R {font-family: MJXc-TeX-sans-R,MJXc-TeX-sans-Rw}"
				".MJXc-TeX-sans-R {font-family: Arial; font-size: 15px; }" .
			Text.replace
				".MJXc-TeX-type-R {font-family: MJXc-TeX-type-R,MJXc-TeX-type-Rw}"
				".MJXc-TeX-type-R {font-family: Roboto Mono; font-size: 9pt; }" .
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

writeXrefDeltaFiles :: SectionFileStyle -> Draft -> [IO ()]
writeXrefDeltaFiles sfs draft = flip map (xrefDelta draft) $ \(from, to) ->
	writeSectionFile (Text.unpack from) sfs (squareAbbr False from) $
		if to == []
			then "Subclause " ++ squareAbbr False from ++ " was removed."
			else "See " ++ intercalateBuilders ", " (flip render ctx . to) ++ "."
	where ctx = defaultRenderContext{draft=draft, page=XrefDeltaPage}
