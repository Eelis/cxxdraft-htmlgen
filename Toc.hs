{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Toc (writeTocFile) where

import qualified Data.Text as Text
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)
import Prelude hiding ((.), (++), writeFile)
import Render (
	render, secnum, SectionPath(..), Link(..), linkToSection, numberSubsecs,
	fileContent, applySectionFileStyle, url, withPaths, SectionFileStyle(..), outputDir)
import Util
import Load14882 (Figure(..), Table(..), Section(..), LaTeX, Draft(..))

tocSection :: (SectionPath, Section) -> Text
tocSection (sectionPath, Section{..}) =
	xml "div" [("id", render abbreviation)] $
	h Nothing (min 4 $ 1 + length (sectionNums sectionPath)) (
		secnum "" sectionPath ++ " " ++
		render (sectionName ++ " ", (linkToSection TocToSection abbreviation){aClass="abbr_ref"})) ++
	mconcat (tocSection . numberSubsecs sectionPath subsections)

tocChapter :: (SectionPath, Section) -> Text
tocChapter (sectionPath, Section{..}) =
	xml "div" [("id", render abbreviation)] $
	h Nothing (min 4 $ 1 + length (sectionNums sectionPath)) (
		secnum "" sectionPath ++ " " ++
		render (sectionName ++ " ", anchor{
			aClass = "folded_abbr_ref",
			aText  = "[" ++ render abbreviation ++ "]",
			aHref  = "#" ++ render abbreviation}) ++
		render (linkToSection TocToSection abbreviation){aClass="unfolded_abbr_ref"}) ++
	xml "div" [("class", "tocChapter")] (mconcat (tocSection . numberSubsecs sectionPath subsections))

listOfTables :: [Table] -> Text
listOfTables tables =
	xml "div" [("id", "tables")] $
		"<h2><a href='#tables'>List of Tables</a></h2>"
		++ xml "div" [("class", "tocChapter")] (mconcat (tableItem . tables))
	where
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
	xml "div" [("id", "figures")] $
		"<h2><a href='#figures'>List of Figures</a></h2>"
		++ xml "div" [("class", "tocChapter")] (mconcat (figureItem . figures))
	where
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
			mconcat (tocChapter . withPaths chapters)
