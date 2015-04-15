{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, OverloadedStrings #-}

import Render (imgDir, outputDir, SectionFileStyle(..))
import Load14882 (Draft(..), load14882)
import Prelude hiding ((++), (.), writeFile)
import System.Directory (createDirectoryIfMissing, copyFile, setCurrentDirectory, getCurrentDirectory)
import System.Environment (getArgs)
import Util

import Toc (writeTocFile)
import SectionPages (writeSectionFiles, writeFullFile, writeFiguresFile, writeTablesFile, writeIndexFiles)

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
	createDirectoryIfMissing True (outputDir ++ imgDir)
	copyFile "14882.css" (outputDir ++ "/14882.css")
	writeTocFile sectionFileStyle draft
	writeIndexFiles sectionFileStyle index
	writeFiguresFile sectionFileStyle figures
	writeTablesFile sectionFileStyle tables
	writeFullFile sectionFileStyle chapters
	writeSectionFiles sectionFileStyle chapters
