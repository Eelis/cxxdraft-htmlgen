{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns #-}

module SectionPages
	( writeSectionFiles
	, writeFullFile
	, writeFiguresFile
	, writeTablesFile
	, writeIndexFiles
	, writeFootnotesFile
	) where

import Prelude hiding ((++), (.), writeFile)
import System.Directory (createDirectoryIfMissing)
import System.IO (hFlush, stdout)
import Control.Monad (forM_)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Render
import Document
import Util

renderParagraph :: Paragraph -> RenderContext -> Text
renderParagraph Paragraph{..} ctx =
		(case paraNumber of
			Just (flip render ctx -> i) -> renderNumbered i
			Nothing -> id)
		$ (if paraInItemdescr then xml "div" [("class", "itemdescr")] else id)
		$ (render paraElems ctx'{extraIndentation=if paraInItemdescr then 3 else 0})
	where
		renderNumbered n =
			let
				idTag = if isJust (page ctx) then [("id", idPrefix ctx ++ n)] else []
				a = anchor
					{ aClass = "marginalized"
					, aHref  =
						if isJust (page ctx)
							then "#" ++ idPrefix ctx ++ n
							else "SectionToSection/" ++ url (abbreviation paraSection) ++ "#" ++ n
					, aText  = n }
			in
				xml "div" (("class", "para") : idTag) .
				(xml "div" [("class", "marginalizedparent")] (render a ctx') ++)
		ctx' = case paraNumber of
			Just (flip render ctx -> n) -> ctx{ idPrefix = idPrefix ctx ++ n ++ "." }
			Nothing -> ctx

parentLink :: Section -> Section -> Text
parentLink parent child
	| Just sub <- Text.stripPrefix (simpleRender (abbreviation parent) ++ ".") secname = sub
	| otherwise = secname
	where
		secname = simpleRender (abbreviation child)

renderSection :: RenderContext -> Maybe Section -> Bool -> Section -> (Text, Bool)
renderSection context specific parasEmitted s@Section{..}
	| full = (, True) $
		xml "div" [("id", secOnPage)] $ header ++
		mconcat (map
			(\p -> renderParagraph p (context{idPrefix=if parasEmitted then secOnPage ++ "-" else ""}))
			paragraphs) ++
		mconcat (fst . renderSection context Nothing True . subsections)
	| not anysubcontent = ("", False)
	| otherwise =
		( header ++
		  mconcat (fst . renderSection context specific False . subsections)
		, anysubcontent )
	where
		secOnPage :: Text
		secOnPage = case page context of
			Just parent -> parentLink parent s
			Nothing -> simpleRender (Document.abbreviation s)
		full = specific == Nothing || specific == Just s
		header = sectionHeader (min 4 $ 1 + length parents) s
			(if specific == Nothing then "#" ++ secOnPage else "")
			abbr
		abbr
			| specific == Just s && not (null parents)
				= anchor
			| Just sp <- specific, sp /= s, not (null parents)
				= anchor{aHref = "SectionToSection/" ++ url abbreviation ++ "#" ++ parentLink s sp}
			| otherwise = linkToSection
					(if null parents then SectionToToc else SectionToSection)
					abbreviation
		anysubcontent =
			or $ map (snd . renderSection context specific True)
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
	simpleRender sectionName ++ " " ++
	simpleRender abbr_ref{aClass = "abbr_ref", aText = squareAbbr abbreviation}

writeFiguresFile :: SectionFileStyle -> [Figure] -> IO ()
writeFiguresFile sfs figs = writeSectionFile "fig" sfs "14882: Figures" $
	"<h1>List of Figures <a href='SectionToToc/fig' class='abbr_ref'>[fig]</a></h1>"
	++ mconcat (r . figs)
	where
		r :: Figure -> Text
		r f@Figure{figureSection=s@Section{..}, ..} =
			"<hr>" ++
			sectionHeader 4 s "" anchor{
				aHref = "SectionToSection/" ++ url abbreviation
					++ "#" ++ url figureAbbr }
			++ renderFig True f

writeTablesFile :: SectionFileStyle -> Draft -> IO ()
writeTablesFile sfs draft = writeSectionFile "tab" sfs "14882: Tables" $
	"<h1>List of Tables <a href='SectionToToc/tab' class='abbr_ref'>[tab]</a></h1>"
	++ mconcat (r . tables draft)
	where
		r :: Table -> Text
		r t@Table{tableSection=s@Section{..}, ..} =
			"<hr>" ++
			sectionHeader 4 s "" (linkToRemoteTable t)
			++ renderTab True t defaultRenderContext{draft=draft}

writeFootnotesFile :: SectionFileStyle -> Draft -> IO ()
writeFootnotesFile sfs draft = writeSectionFile "footnotes" sfs "14882: Footnotes" $
	"<h1>List of Footnotes</h1>"
	++ mconcat (r . footnotes draft)
	where
		r :: Footnote -> Text
		r fn = simpleRender fn

writeFullFile :: SectionFileStyle -> Draft -> IO ()
writeFullFile sfs draft = do
	putStrLn "  full"
	writeSectionFile "full" sfs "14882" $
		mconcat $ applySectionFileStyle sfs . fst .
			renderSection defaultRenderContext{draft=draft} Nothing True . chapters draft

writeSectionFiles :: SectionFileStyle -> Draft -> IO ()
writeSectionFiles sfs draft = do
	putStr "  sections..";
	let secs = Document.sections draft
	forM_ secs $ \section@Section{..} -> do
		putStr "."; hFlush stdout
		writeSectionFile (Text.unpack $ abbrAsPath abbreviation) sfs (squareAbbr abbreviation) $
			(mconcat $ fst . renderSection (defaultRenderContext{draft=draft,page=Just section}) (Just section) False . chapters draft)
	putStrLn $ " " ++ show (length secs)

writeIndexFiles :: SectionFileStyle -> Index -> IO ()
writeIndexFiles sfs index = forM_ (Map.toList index) $ \(Text.unpack -> cat, i) -> do
	putStrLn $ "  " ++ cat
	writeSectionFile cat sfs ("14882: " ++ indexCatName cat) $ h 1 (indexCatName cat) ++ simpleRender i
