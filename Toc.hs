{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns, NamedFieldPuns #-}

module Toc (writeTocFile) where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Prelude hiding ((.), (++), writeFile)
import LaTeXBase (LaTeXUnit(..))
import Render (
	secnum, Link(..), linkToSection, simpleRender2, Page(TocPage), RenderContext(..), render,
	fileContent, applySectionFileStyle, SectionFileStyle(..), outputDir, defaultRenderContext)
import Util
import Document (Section(..), Draft(..), SectionKind(..), indexCatName, isDefinitionSection)

tocSection :: Section -> TextBuilder.Builder
tocSection Section{sectionKind=DefinitionSection _} = ""
tocSection s@Section{..} =
	xml "div" [("id", abbreviation)] $
	h (min 4 $ 2 + length parents) (
		secnum "" s ++ " " ++
		render ( sectionName ++ [TeXRaw " "]
		       , (linkToSection TocToSection abbreviation){aClass="abbr_ref"}) defaultRenderContext{page=TocPage, inSectionTitle=True}) ++
	mconcat (tocSection . subsections)

tocChapter :: Section -> TextBuilder.Builder
tocChapter s@Section{abbreviation, sectionName, subsections, parents} =
	xml "div" [("id", abbreviation)] $
	h (min 4 $ 2 + length parents) (
		secnum "" s ++ " " ++
		render (sectionName ++ [TeXRaw " "], link) defaultRenderContext{inSectionTitle=True} ++
		simpleRender2 (linkToSection TocToSection abbreviation){aClass="unfolded_abbr_ref"}) ++
	xml "div" [("class", "tocChapter")] (mconcat (tocSection . subsections))
  where
	href = (if any (not . isDefinitionSection . sectionKind) subsections then "#" else "TocToSection/")
	    ++ urlChars abbreviation
	link = anchor{
		aClass = "folded_abbr_ref",
		aText = TextBuilder.fromText $ "[" ++ abbreviation ++ "]",
		aHref = href}

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

writeTocFile :: SectionFileStyle -> Draft -> IO ()
writeTocFile sfs Draft{..} = do
	putStrLn "  toc"
	date <- getCurrentTime
	tocCss <- readFile "toc.css"
	let
	    descMeta = "<meta name='description' content='Browser-friendly rendering of a recent draft of the C++ standard'>"
	    tocStyle = "<style>" ++ TextBuilder.fromString tocCss ++ "</style>"
	writeFile (outputDir ++ "/index.html") $ applySectionFileStyle sfs $ LazyText.toStrict $ TextBuilder.toLazyText $
		fileContent "" "Draft C++ Standard: Contents" (descMeta ++ tocStyle) $
			"<h1 style='text-align:center; hyphens:none'>Working Draft, Standard for Programming Language C++</h1>" ++
			"<br>" ++ xml "div" [("class", "tocHeader")] (TextBuilder.fromText $ tocHeader date commitUrl) ++
			"<br><h1>Contents</h1>" ++
			mconcat (tocChapter . chapters) ++
			mconcat (h 2
				. (\cat -> simpleRender2 anchor{aHref="TocToSection/" ++ cat, aText=indexCatName cat})
				. ["generalindex", "headerindex", "libraryindex", "impldefindex"])
