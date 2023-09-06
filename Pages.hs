{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Pages (fileContent, pageContent, pagePath, writePage, applyPageStyle, Link(..), outputDir, PageStyle(..)) where

import Prelude hiding ((++), (.), writeFile)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (when)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import Util ((++), (.), Text, writeFile)

outputDir :: FilePath
outputDir = "14882/"

data PageStyle = Bare | WithExtension | InSubdir
    deriving (Eq, Read)

fileContent :: TextBuilder.Builder -> TextBuilder.Builder -> TextBuilder.Builder -> TextBuilder.Builder -> TextBuilder.Builder
fileContent pathHome title extraHead body =
    "<!DOCTYPE html>" ++
    "<html lang='en'>" ++
        "<head>" ++
            "<title>" ++ title ++ "</title>" ++
            "<meta charset='UTF-8'/>" ++
            "<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "14882.css'/>" ++
            "<link rel='stylesheet' type='text/css' href='https://fonts.googleapis.com/css2?family=Noto+Serif'/>" ++
            "<link rel='stylesheet' type='text/css' href='https://fonts.googleapis.com/css2?family=Noto+Sans'/>" ++
            "<link rel='stylesheet' type='text/css' href='https://fonts.googleapis.com/css2?family=Noto+Sans+Mono'/>" ++
            "<link rel='icon' href='icon.png'/>" ++
            extraHead ++
        "</head>" ++
        "<body><div class='wrapper'>" ++ body ++ "</div></body>" ++
    "</html>"

data Link = TocToSection | SectionToToc | SectionToSection
	deriving Show

doLink :: PageStyle -> Link -> Text -> Text
doLink sfs l = LazyText.toStrict . TextBuilder.toLazyText . go . Text.splitOn (Text.pack (show l) ++ "/")
	where
		go :: [Text] -> TextBuilder.Builder
		go (x : (Text.break (`elem` ("'#" :: String)) -> (a, b)) : z) = TextBuilder.fromText x ++ f (TextBuilder.fromText a) ++ go (b : z)
		go [x] = TextBuilder.fromText x
		go _ = undefined
		f :: TextBuilder.Builder -> TextBuilder.Builder
		f = case (sfs, l) of
			(Bare, SectionToToc) -> ("./#" ++)
			(Bare, TocToSection) -> id
			(Bare, SectionToSection) -> id
			(InSubdir, SectionToToc) -> ("../#" ++)
			(InSubdir, TocToSection) -> (++ "/")
			(InSubdir, SectionToSection) -> ("../" ++)
			(WithExtension, SectionToToc) -> ("index.html#" ++)
			(WithExtension, TocToSection) -> (++ ".html")
			(WithExtension, SectionToSection) -> (++ ".html")

applyPageStyle :: PageStyle -> Text -> Text
applyPageStyle sfs =
	doLink sfs SectionToSection
	. doLink sfs SectionToToc
	. doLink sfs TocToSection

pagePath :: FilePath -> PageStyle -> String
pagePath n Bare = outputDir ++ n
pagePath n WithExtension = outputDir ++ n ++ ".html"
pagePath n InSubdir = outputDir ++ n ++ "/index.html"

pageContent :: PageStyle -> TextBuilder.Builder -> Text
pageContent sfs content = applyPageStyle sfs $ LazyText.toStrict $ TextBuilder.toLazyText $ content

writePage :: FilePath -> PageStyle -> Text -> IO ()
writePage n sfs content = do
    when (sfs == InSubdir) $ createDirectoryIfMissing True (outputDir ++ n)
    writeFile (pagePath n sfs) content
