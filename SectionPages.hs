{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns #-}

module SectionPages (writeSectionFiles, writeFullFile) where

import Prelude hiding ((++), (.), writeFile)
import System.Directory (createDirectoryIfMissing)
import System.IO (hFlush, stdout)
import Control.Monad (forM_)
import qualified Data.Text as Text
import Render
import Load14882
import Util

renderParagraph :: Text -> (Int, Paragraph) -> Text
renderParagraph idPrefix (render -> i, x) =
	xml "div" [("class", "para"), ("id", idPrefix ++ i)] $
	xml "div" [("class", "marginalizedparent")]
		(render (anchor{aClass="marginalized", aHref="#" ++ idPrefix ++ i,aText=i})) ++
	render x

renderChapter :: Maybe LaTeX -> Bool -> (SectionPath, Section) -> (Text, Bool)
renderChapter specific parasEmitted p@(_, Section{abbreviation=chapter}) =
	renderSection chapter specific parasEmitted p

renderSection :: LaTeX -> Maybe LaTeX -> Bool -> (SectionPath, Section) -> (Text, Bool)
renderSection chapter specific parasEmitted (path@SectionPath{..}, Section{..})
	| full = (, True) $
		xml "div" [("id", render abbreviation)] $ header ++
		xml "div" [("class", "para")] (render preamble) ++
		mconcat (map
			(renderParagraph (if parasEmitted then url abbreviation ++ "-" else ""))
			(zip [1..] paragraphs)) ++
		mconcat (fst . renderSection chapter Nothing True . numberSubsecs path subsections)
	| not anysubcontent = ("", False)
	| otherwise =
		( header ++
		  mconcat (fst . renderSection chapter specific False . numberSubsecs path subsections)
		, anysubcontent )
	where
		full = specific == Nothing || specific == Just abbreviation
		header = h Nothing (min 4 $ length sectionNums) $
			secnum (if specific == Nothing then "#" ++ url abbreviation else "") path ++ " "
			++ render sectionName ++ " "
			++ render abbr{aClass="abbr_ref", aText="[" ++ render abbreviation ++ "]"}
		abbr
			| specific == Just abbreviation && abbreviation /= chapter
				= anchor
			| Just s <- specific, s /= abbreviation, abbreviation /= chapter
				= anchor{aHref = "SectionToSection/" ++ url abbreviation ++ "#" ++ url s}
			| otherwise = linkToSection
					(if abbreviation == chapter then SectionToToc else SectionToSection)
					abbreviation
		anysubcontent =
			or $ map (snd . renderSection chapter specific True)
			   $ numberSubsecs path subsections

fullFileContent :: SectionFileStyle -> [Chapter] -> Text
fullFileContent sfs chapters = applySectionFileStyle sfs $
	fileContent (if sfs == InSubdir then "../" else "") "14882" $
		mconcat $ applySectionFileStyle sfs . fst . renderChapter Nothing True . withPaths chapters

writeFullFile :: SectionFileStyle -> [Chapter] -> IO ()
writeFullFile sfs chapters = do
	fullFile <- case sfs of
		Bare -> return "full"
		WithExtension -> return "full.html"
		InSubdir -> do
			createDirectoryIfMissing True (outputDir ++ "/full")
			return "full/index.html"
	writeFile (outputDir ++ fullFile) $ fullFileContent sfs chapters

sectionFileContent :: SectionFileStyle -> [Chapter] -> LaTeX -> Text
sectionFileContent sfs chapters abbreviation = applySectionFileStyle sfs $
	fileContent
		(if sfs == InSubdir then "../" else "")
		("[" ++ render abbreviation ++ "]")
		(mconcat $ fst . renderChapter (Just abbreviation) False . withPaths chapters)

writeSectionFiles :: SectionFileStyle -> [Chapter] -> IO ()
writeSectionFiles sfs chapters = do
	let allAbbrs = concatMap abbreviations (snd . chapters)
	forM_ allAbbrs $ \abbreviation -> do
		putStr "."; hFlush stdout
		f <- case sfs of
			Bare -> return $ Text.unpack $ abbrAsPath abbreviation
			WithExtension -> return $ Text.unpack $ abbrAsPath abbreviation ++ ".html"
			InSubdir -> do
				let dir = "/" ++ Text.unpack (abbrAsPath abbreviation)
				createDirectoryIfMissing True (outputDir ++ dir)
				return $ dir ++ "/index.html"
		writeFile (outputDir ++ f) $ sectionFileContent sfs chapters abbreviation
	putStrLn $ " " ++ show (length allAbbrs) ++ " sections"
