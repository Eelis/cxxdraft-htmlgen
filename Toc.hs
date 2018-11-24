{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}

module Toc (writeTocFile) where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Prelude hiding ((.), (++), writeFile)
import LaTeXBase (LaTeXUnit(..))
import Render (
	secnum, Link(..), linkToSection, simpleRender, simpleRender2, squareAbbr, Page(TocPage), RenderContext(..), render,
	fileContent, applySectionFileStyle, url, SectionFileStyle(..), outputDir, defaultRenderContext)
import Util
import Document (Figure(..), Table(..), Section(..), Draft(..), SectionKind(..), indexCatName, figures, tables)

tocSection :: Section -> TextBuilder.Builder
tocSection Section{sectionKind=DefinitionSection _} = ""
tocSection s@Section{..} =
	xml "div" [("id", simpleRender abbreviation)] $
	h (min 4 $ 2 + length parents) (
		secnum "" s ++ " " ++
		render ( sectionName ++ [TeXRaw " "]
		       , (linkToSection TocToSection abbreviation){aClass="abbr_ref"}) defaultRenderContext{page=TocPage, inSectionTitle=True}) ++
	mconcat (tocSection . subsections)

tocChapter :: Section -> TextBuilder.Builder
tocChapter s@Section{..} =
	xml "div" [("id", simpleRender abbreviation)] $
	h (min 4 $ 2 + length parents) (
		secnum "" s ++ " " ++
		render (sectionName ++ [TeXRaw " "], anchor{
			aClass = "folded_abbr_ref",
			aText  = "[" ++ simpleRender2 abbreviation ++ "]",
			aHref  = "#" ++ simpleRender abbreviation}) defaultRenderContext{inSectionTitle=True} ++
		simpleRender2 (linkToSection TocToSection abbreviation){aClass="unfolded_abbr_ref"}) ++
	xml "div" [("class", "tocChapter")] (mconcat (tocSection . subsections))

listOfTables :: [Table] -> TextBuilder.Builder
listOfTables ts =
	xml "div" [("id", "tab")] $
		h 2 ("List of Tables "
			++ simpleRender2 abbrAnchor{aHref="#tab", aClass="folded_abbr_ref"}
			++ simpleRender2 abbrAnchor{aHref="TocToSection/tab", aClass="unfolded_abbr_ref"})
		++ xml "div" [("class", "tocChapter")] (mconcat (tableItem . ts))
	where
		abbrAnchor = anchor{aText="[tab]"}
		tableItem :: Table -> TextBuilder.Builder
		tableItem Table{..} =
			spanTag "secnum" (simpleRender2 tableNumber)
			++ simpleRender2 tableCaption
			++ simpleRender2 anchor{
				aHref  = "TocToSection/" ++ url (abbreviation tableSection)
				         ++ "#" ++ url (head tableAbbrs),
				aText  = squareAbbr (head tableAbbrs),
				aClass = "abbr_ref"}
			++ "<br>"

listOfFigures :: [Figure] -> TextBuilder.Builder
listOfFigures figs =
	xml "div" [("id", "fig")] $
		h 2 ("List of Figures "
			++ simpleRender2 abbrAnchor{aHref="#fig", aClass="folded_abbr_ref"}
			++ simpleRender2 abbrAnchor{aHref="TocToSection/fig", aClass="unfolded_abbr_ref"})
		++ xml "div" [("class", "tocChapter")] (mconcat (figureItem . figs))
	where
		abbrAnchor = anchor{aText="[fig]"}
		figureItem :: Figure -> TextBuilder.Builder
		figureItem Figure{..} =
			spanTag "secnum" (simpleRender2 figureNumber)
			++ simpleRender2 figureName
			++ simpleRender2 anchor{
				aHref  = "TocToSection/" ++ url (abbreviation figureSection)
				         ++ "#" ++ url figureAbbr,
				aText  = squareAbbr figureAbbr,
				aClass = "abbr_ref"}
			++ "<br>"

tocHeader :: UTCTime -> Text -> Text
tocHeader date commitUrl =
	"Generated on " ++ Text.pack (formatTime defaultTimeLocale "%F" date)
	++ " from the C++ standard's <a href='" ++ commitUrl ++ "'>draft LaTeX sources</a>"
	++ " by <a href='https://github.com/Eelis/cxxdraft-htmlgen'>cxxdraft-htmlgen</a>."
	++ " This is <em>not</em> an ISO publication."
	++ "<hr/>"

writeTocFile :: SectionFileStyle -> Draft -> IO ()
writeTocFile sfs draft@Draft{..} = do
	putStrLn "  toc"
	date <- getCurrentTime
	tocCss <- readFile "toc.css"
	let
	    descMeta = "<meta name='description' content='Browser-friendly rendering of a recent draft of the C++ standard'>"
	    tocStyle = "<style>" ++ TextBuilder.fromString tocCss ++ "</style>"
	writeFile (outputDir ++ "/index.html") $ applySectionFileStyle sfs $ LazyText.toStrict $ TextBuilder.toLazyText $
		fileContent "" "Draft C++ Standard: Contents" (descMeta ++ tocStyle) $
			xml "div" [("class", "tocHeader")] (TextBuilder.fromText $ tocHeader date commitUrl) ++
			"<h1>Contents</h1>" ++
			listOfTables (snd . tables draft) ++
			listOfFigures (figures draft) ++
			mconcat (tocChapter . chapters) ++
			mconcat (h 2
				. (\cat -> simpleRender2 anchor{aHref="TocToSection/" ++ cat, aText=indexCatName cat})
				. ["generalindex", "headerindex", "libraryindex", "impldefindex"])
