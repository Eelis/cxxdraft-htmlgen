{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, OverloadedStrings #-}

import Render (outputDir, SectionFileStyle(..))
import Document (Draft(..), figures)
import Load14882 (load14882)
import Prelude hiding ((++), (.), writeFile)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory, getCurrentDirectory, copyFile)
import System.Environment (getArgs)
import Util

import Toc (writeTocFile)
import SectionPages
	( writeSectionFiles, writeFullFile, writeFiguresFile, writeTablesFile
	, writeIndexFiles, writeFootnotesFile, writeCssFile)

data CmdLineArgs = CmdLineArgs
	{ repo :: FilePath
	, sectionFileStyle :: SectionFileStyle }

readCmdLineArgs :: [String] -> CmdLineArgs
readCmdLineArgs = \case
	[repo, read -> sectionFileStyle] -> CmdLineArgs{..}
	[repo] -> CmdLineArgs{sectionFileStyle=WithExtension,..}
	_ -> error "param: path/to/repo"

main :: IO ()
main = do
	cwd <- getCurrentDirectory
	CmdLineArgs{..} <- readCmdLineArgs . getArgs

	setCurrentDirectory $ repo ++ "/source"
	draft@Draft{..} <- load14882

	setCurrentDirectory cwd
	putStrLn $ "Writing to " ++ outputDir
	createDirectoryIfMissing True outputDir
	copyFile "icon.png" (outputDir ++ "/icon.png")
	writeCssFile
	writeTocFile sectionFileStyle draft
	writeIndexFiles sectionFileStyle index
	writeFiguresFile sectionFileStyle (figures draft)
	writeTablesFile sectionFileStyle draft
	writeFootnotesFile sectionFileStyle draft
	writeFullFile sectionFileStyle draft
	writeSectionFiles sectionFileStyle draft
