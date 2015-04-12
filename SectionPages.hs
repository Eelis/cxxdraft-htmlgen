{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns #-}

module SectionPages (writeSectionFiles, writeFullFile, writeFiguresFile, writeTablesFile) where

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

writeSectionFile :: FilePath -> SectionFileStyle -> Text -> Text -> IO ()
writeSectionFile n sfs title body = do
	file <- case sfs of
		Bare -> return n
		WithExtension -> return $ n ++ ".html"
		InSubdir -> do
			createDirectoryIfMissing True (outputDir ++ n)
			return $ n ++ "/index.html"
	writeFile (outputDir ++ file) $ applySectionFileStyle sfs $
		fileContent (if sfs == InSubdir then "../" else "") title body

writeFiguresFile :: SectionFileStyle -> [Figure] -> IO ()
writeFiguresFile sfs figures = writeSectionFile "figures" sfs "14882: Figures" $
	"<h1>List of Figures</h1>" ++ mconcat (render . figures)

writeTablesFile :: SectionFileStyle -> [Table] -> IO ()
writeTablesFile sfs tables = writeSectionFile "tables" sfs "14882: Tables" $
	"<h1>List of Tables</h1>" ++ mconcat (r . tables)
	where
		r :: Table -> Text
		r t@Table{tableSection=Section{..}, ..} =
			h Nothing 2
				(render sectionName ++ " " ++
				render anchor
					{ aHref  = "SectionToSection/" ++ url abbreviation
						++ "#" ++ replace ":" "-" (url $ head tableAbbrs)
					, aClass = "abbr_ref"
					, aText  = "[" ++ render abbreviation ++ "]" })
			++ render t

writeFullFile :: SectionFileStyle -> [Chapter] -> IO ()
writeFullFile sfs chapters = writeSectionFile "full" sfs "14882" $
	mconcat $ applySectionFileStyle sfs . fst . renderChapter Nothing True . withPaths chapters

writeSectionFiles :: SectionFileStyle -> [Chapter] -> IO ()
writeSectionFiles sfs chapters = do
	let allAbbrs = concatMap abbreviations (snd . chapters)
	forM_ allAbbrs $ \abbreviation -> do
		putStr "."; hFlush stdout
		writeSectionFile (Text.unpack $ abbrAsPath abbreviation) sfs ("[" ++ render abbreviation ++ "]") $
			(mconcat $ fst . renderChapter (Just abbreviation) False . withPaths chapters)
	putStrLn $ " " ++ show (length allAbbrs) ++ " sections"
