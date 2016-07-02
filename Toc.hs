{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}

module Toc (writeTocFile) where

import qualified Data.Text as Text
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)
import Prelude hiding ((.), (++), writeFile)
import Render (
	render, secnum, Link(..), linkToSection,
	fileContent, applySectionFileStyle, url, SectionFileStyle(..), outputDir)
import Util
import Load14882 (Figure(..), Table(..), Section(..), Draft(..), indexCatName)

tocSection :: Section -> Text
tocSection s@Section{..} =
	xml "div" [("id", render abbreviation)] $
	h (min 4 $ 2 + length parents) (
		secnum "" s ++ " " ++
		render (sectionName ++ " ", (linkToSection TocToSection abbreviation){aClass="abbr_ref"})) ++
	mconcat (tocSection . subsections)

tocChapter :: Section -> Text
tocChapter s@Section{..} =
	xml "div" [("id", render abbreviation)] $
	h (min 4 $ 2 + length parents) (
		secnum "" s ++ " " ++
		render (sectionName ++ " ", anchor{
			aClass = "folded_abbr_ref",
			aText  = "[" ++ render abbreviation ++ "]",
			aHref  = "#" ++ render abbreviation}) ++
		render (linkToSection TocToSection abbreviation){aClass="unfolded_abbr_ref"}) ++
	xml "div" [("class", "tocChapter")] (mconcat (tocSection . subsections))

listOfTables :: [Table] -> Text
listOfTables tables =
	xml "div" [("id", "tab")] $
		h 2 ("List of Tables "
			++ render abbrAnchor{aHref="#tab", aClass="folded_abbr_ref"}
			++ render abbrAnchor{aHref="TocToSection/tab", aClass="unfolded_abbr_ref"})
		++ xml "div" [("class", "tocChapter")] (mconcat (tableItem . tables))
	where
		abbrAnchor = anchor{aText="[tab]"}
		tableItem :: Table -> Text
		tableItem Table{..} =
			spanTag "secnum" (render tableNumber)
			++ render tableCaption
			++ render anchor{
				aHref  = "TocToSection/" ++ url (abbreviation tableSection)
				         ++ "#" ++ replace ":" "-" (render $ head tableAbbrs),
				aText  = "[" ++ render (head tableAbbrs) ++ "]",
				aClass = "abbr_ref"}
			++ "<br>"

listOfFigures :: [Figure] -> Text
listOfFigures figures =
	xml "div" [("id", "fig")] $
		h 2 ("List of Figures "
			++ render abbrAnchor{aHref="#fig", aClass="folded_abbr_ref"}
			++ render abbrAnchor{aHref="TocToSection/fig", aClass="unfolded_abbr_ref"})
		++ xml "div" [("class", "tocChapter")] (mconcat (figureItem . figures))
	where
		abbrAnchor = anchor{aText="[fig]"}
		figureItem :: Figure -> Text
		figureItem Figure{..} =
			spanTag "secnum" (render figureNumber)
			++ render figureName
			++ render anchor{
				aHref  = "TocToSection/" ++ url (abbreviation figureSection)
				         ++ "#" ++ replace ":" "-" (render figureAbbr),
				aText  = "[" ++ render figureAbbr ++ "]",
				aClass = "abbr_ref"}
			++ "<br>"

writeTocFile :: SectionFileStyle -> Draft -> IO ()
writeTocFile sfs Draft{..} = do
	putStrLn "  toc"
	date <- Text.pack . formatTime defaultTimeLocale "%F" . getCurrentTime
	writeFile (outputDir ++ "/index.html") $ applySectionFileStyle sfs $
		fileContent "" "14882: Contents" $
			xml "div" [("class", "tocHeader")]
				( "Generated on " ++ date
				++ " from the C++ standard's <a href='" ++ commitUrl ++ "'>draft LaTeX sources</a>"
				++ " by <a href='https://github.com/Eelis/cxxdraft-htmlgen'>cxxdraft-htmlgen</a>."
				++ "<hr/>") ++
			"<h1>Contents</h1>" ++
			listOfTables tables ++
			listOfFigures figures ++
			mconcat (tocChapter . chapters) ++
			mconcat (h 2
				. (\cat -> render anchor{aHref="TocToSection/" ++ cat, aText=indexCatName cat})
				. ["generalindex", "libraryindex", "impldefindex"])
