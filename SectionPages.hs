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

renderSection :: Maybe LaTeX -> Bool -> Section -> (Text, Bool)
renderSection specific parasEmitted s@Section{..}
	| full = (, True) $
		xml "div" [("id", render abbreviation)] $ header ++
		xml "div" [("class", "para")] (render preamble) ++
		mconcat (map
			(renderParagraph (if parasEmitted then url abbreviation ++ "-" else ""))
			(zip [1..] paragraphs)) ++
		mconcat (fst . renderSection Nothing True . subsections)
	| not anysubcontent = ("", False)
	| otherwise =
		( header ++
		  mconcat (fst . renderSection specific False . subsections)
		, anysubcontent )
	where
		full = specific == Nothing || specific == Just abbreviation
		header = sectionHeader (min 4 $ 1 + length parents) s
			(if specific == Nothing then "#" ++ url abbreviation else "")
			abbr
		abbr
			| specific == Just abbreviation && not (null parents)
				= anchor
			| Just sp <- specific, sp /= abbreviation, not (null parents)
				= anchor{aHref = "SectionToSection/" ++ url abbreviation ++ "#" ++ url sp}
			| otherwise = linkToSection
					(if null parents then SectionToToc else SectionToSection)
					abbreviation
		anysubcontent =
			or $ map (snd . renderSection specific True)
			   $ subsections

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

sectionHeader :: Int -> Section -> Text -> Anchor -> Text
sectionHeader hLevel s@Section{..} secnumHref abbr_ref = h hLevel $
	secnum secnumHref s ++ " " ++
	render sectionName ++ " " ++
	render abbr_ref{aClass = "abbr_ref", aText = "[" ++ render abbreviation ++ "]"}

writeFiguresFile :: SectionFileStyle -> [Figure] -> IO ()
writeFiguresFile sfs figures = writeSectionFile "fig" sfs "14882: Figures" $
	"<h1>List of Figures <a href='SectionToToc/fig' class='abbr_ref'>[fig]</a></h1>"
	++ mconcat (r . figures)
	where
		r :: Figure -> Text
		r f@Figure{figureSection=s@Section{..}, ..} =
			"<hr>" ++
			sectionHeader 4 s "" anchor{
				aHref = "SectionToSection/" ++ url abbreviation
					++ "#" ++ replace ":" "-" (url figureAbbr) }
			++ renderFig True f

writeTablesFile :: SectionFileStyle -> [Table] -> IO ()
writeTablesFile sfs tables = writeSectionFile "tab" sfs "14882: Tables" $
	"<h1>List of Tables <a href='SectionToToc/tab' class='abbr_ref'>[tab]</a></h1>"
	++ mconcat (r . tables)
	where
		r :: Table -> Text
		r t@Table{tableSection=s@Section{..}, ..} =
			"<hr>" ++
			sectionHeader 4 s "" anchor{
				aHref = "SectionToSection/" ++ url abbreviation
					++ "#" ++ replace ":" "-" (url $ head tableAbbrs) }
			++ renderTab True t

writeFullFile :: SectionFileStyle -> [Section] -> IO ()
writeFullFile sfs chapters = writeSectionFile "full" sfs "14882" $
	mconcat $ applySectionFileStyle sfs . fst . renderSection Nothing True . chapters

writeSectionFiles :: SectionFileStyle -> [Section] -> IO ()
writeSectionFiles sfs chapters = do
	let allAbbrs = concatMap abbreviations chapters
	forM_ allAbbrs $ \abbreviation -> do
		putStr "."; hFlush stdout
		writeSectionFile (Text.unpack $ abbrAsPath abbreviation) sfs ("[" ++ render abbreviation ++ "]") $
			(mconcat $ fst . renderSection (Just abbreviation) False . chapters)
	putStrLn $ " " ++ show (length allAbbrs) ++ " sections"
