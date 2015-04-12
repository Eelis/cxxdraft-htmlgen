{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, OverloadedStrings #-}

import Render
import Load14882 (Draft(..), load14882)
import Prelude hiding ((++), (.), writeFile)
import System.IO (hFlush, stdout)
import Control.Monad (forM_)
import System.Directory (createDirectoryIfMissing, copyFile, setCurrentDirectory, getCurrentDirectory)
import qualified Data.Text as Text
import System.Environment (getArgs)
import Data.Text.IO (writeFile)
import Util

import Toc (tocFileContent)
import SectionPages (sectionFileContent, fullFileContent)

writeStuff :: SectionFileStyle -> Draft -> IO ()
writeStuff sfs d@Draft{..} = do
	putStr $ "Writing to " ++ outputDir; hFlush stdout

	createDirectoryIfMissing True outputDir
	createDirectoryIfMissing True (outputDir ++ imgDir)

	copyFile "14882.css" (outputDir ++ "/14882.css")

	writeFile (outputDir ++ "/index.html") $ tocFileContent sfs d

	fullFile <- case sfs of
		Bare -> return "full"
		WithExtension -> return "full.html"
		InSubdir -> do
			createDirectoryIfMissing True (outputDir ++ "/full")
			return "full/index.html"
	writeFile (outputDir ++ fullFile) $ fullFileContent sfs chapters

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
	
	draft <- load14882

	setCurrentDirectory cwd
	writeStuff sectionFileStyle draft
