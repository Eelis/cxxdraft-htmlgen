{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns, NamedFieldPuns #-}

module Toc (writeTocFiles) where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Prelude hiding ((.), (++), writeFile)
import LaTeXBase (LaTeXUnit(..))
import Pages (Link(..), fileContent, applyPageStyle, PageStyle(..), outputDir, writePage)
import Render (secnum, linkToSection, simpleRender2, RenderContext(..), render, defaultRenderContext, Page(..))
import Util
import Document (Section(..), Draft(..), SectionKind(..), indexCatName, isDefinitionSection)

tocSection :: Draft -> Bool -> Section -> TextBuilder.Builder
tocSection _ _ Section{sectionKind=DefinitionSection _} = ""
tocSection draft expanded s@Section{..} =
	xml "div" [("id", abbreviation)] $ header ++ mconcat (tocSection draft expanded . subsections)
  where
  	header = h (min 4 $ 2 + length parents) $
		secnum 0 (if expanded then "#" ++ urlChars abbreviation else "") s ++ " "
		++ render ( sectionName ++ [TeXRaw " "]
		          , (linkToSection (if expanded then SectionToSection else TocToSection) abbreviation){aClass="abbr_ref"})
		          defaultRenderContext{page=if expanded then ExpandedTocPage else TocPage, inSectionTitle=True, draft=draft}
		++ "<div style='clear:right'></div>"

tocChapter :: Draft -> Bool -> Section -> TextBuilder.Builder
tocChapter draft expanded s@Section{abbreviation, sectionName, subsections, parents} =
	xml "div" [("id", abbreviation)] $
	h (min 4 $ 2 + length parents) header ++
	xml "div" [("class", "tocChapter")] (mconcat (tocSection draft expanded . subsections))
  where
	href
	    | expanded = "SectionToSection/" ++ urlChars abbreviation
	    | otherwise = (if any (not . isDefinitionSection . sectionKind) subsections then "#" else "TocToSection/") ++ urlChars abbreviation
	link = anchor{
		aClass = "folded_abbr_ref",
		aText = TextBuilder.fromText $ "[" ++ abbreviation ++ "]",
		aHref = href}
	header
	  | abbreviation == "bibliography" =
	      render anchor{aText = "Bibliography", aHref = href}
	        defaultRenderContext{inSectionTitle=True, draft=draft}
	  | otherwise =
	      secnum 0 (if expanded then "#" ++ urlChars abbreviation else "") s ++ " " ++
	      render (sectionName ++ [TeXRaw " "], link) defaultRenderContext{inSectionTitle=True, draft=draft} ++
	      (if expanded then "" else simpleRender2 (linkToSection TocToSection abbreviation){aClass="unfolded_abbr_ref"})

tocHeader :: UTCTime -> Text -> Text
tocHeader date commitUrl =
	"(Generated on " ++ Text.pack (formatTime defaultTimeLocale "%F" date)
	++ " from the <a href='" ++ commitUrl ++ "'>LaTeX sources</a>"
	++ " by <a href='https://github.com/Eelis/cxxdraft-htmlgen'>cxxdraft-htmlgen</a>."
	++ " This is <em>not</em> an ISO publication.)"
	++ "<br><br>"
	++ "<b>Note: this is an early draft. It's known to be incomplet and incorrekt, and it has lots of"
	++ " b<span style='position:relative;left:-1.2pt'>a</span><span style='position:relative;left:1pt'>d</span>"
	++ " for<span style='position:relative;left:-3pt'>matti<span style='position:relative;bottom:0.15ex'>n</span>g.</span></b>"

writeTocFiles :: PageStyle -> Draft -> IO ()
writeTocFiles sfs draft@Draft{..} = do
	date <- getCurrentTime
	tocCss <- readFile "toc.css"
	let
	    descMeta = "<meta name='description' content='Browser-friendly rendering of a recent draft of the C++ standard'>"
	    tocStyle = "<style>" ++ TextBuilder.fromString tocCss ++ "</style>"
	writeFile (outputDir ++ "/index.html") $ applyPageStyle sfs $ LazyText.toStrict $ TextBuilder.toLazyText $
		fileContent "" "Draft C++ Standard: Contents" (descMeta ++ tocStyle) $
			"<h1 style='text-align:center; hyphens:none; margin: 1cm'>Working Draft<br>Programming Languages &mdash; C++</h1>" ++
			xml "div" [("class", "tocHeader")] (TextBuilder.fromText $ tocHeader date commitUrl) ++
			"<br><h1>Contents</h1>" ++
			mconcat (tocChapter draft False . chapters) ++
			mconcat (h 2
				. (\cat -> simpleRender2 anchor{aHref="TocToSection/" ++ cat, aText=indexCatName cat})
				. ["generalindex", "grammarindex", "headerindex", "libraryindex", "conceptindex", "impldefindex"])

	fullTocCss <- readFile "fulltoc.css"
	let
	    fullTocStyle = "<style>" ++ TextBuilder.fromString fullTocCss ++ "</style>"
	    pathHome = if sfs == InSubdir then "../" else ""
	writePage "fulltoc" sfs $ applyPageStyle sfs $ LazyText.toStrict $ TextBuilder.toLazyText $
		fileContent pathHome "Draft C++ Standard: Contents" (descMeta ++ fullTocStyle) $
			"<h1 style='text-align:center; hyphens:none; margin: 1cm'>Working Draft<br>Programming Languages &mdash; C++</h1>" ++
			xml "div" [("class", "tocHeader")] (TextBuilder.fromText $ tocHeader date commitUrl) ++
			"<br><h1>Contents</h1>" ++
			mconcat (tocChapter draft True . chapters) ++
			mconcat (h 2
				. (\cat -> simpleRender2 anchor{aHref="SectionToSection/" ++ cat, aText=indexCatName cat})
				. ["generalindex", "grammarindex", "headerindex", "libraryindex", "conceptindex", "impldefindex"])
