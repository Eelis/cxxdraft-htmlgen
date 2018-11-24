{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module MathJax (render) where

import Control.Concurrent.MVar (takeMVar, putMVar, newMVar)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)
import System.Process (shell, CreateProcess(..), createProcess, StdStream(CreatePipe))
import System.IO (BufferMode(..), hGetLine, hPutStrLn, hSetBuffering)
import Text.Regex (mkRegex, subRegex)
import Prelude hiding ((++))
import Util ((++))

rmTrailingNewline :: Text -> Text
rmTrailingNewline (Text.stripSuffix "\n" -> Just x) = x
rmTrailingNewline x = x

type Renderer = String {- formula -} -> Bool {- inline -} -> Text

makeRenderer :: IO Renderer
makeRenderer = do

    (Just stdinPipe, Just stdoutPipe, _, _) <- createProcess
        (shell "/home/eelis/projects/c++/htmlstandard/cxxdraft-htmlgen/mathjax-batch")
            {std_in = CreatePipe, std_out = CreatePipe}

    hSetBuffering stdinPipe LineBuffering
    hSetBuffering stdoutPipe LineBuffering

    let
      rm r s = subRegex (mkRegex r) s ""
      readResult = do
        line <- hGetLine stdoutPipe
        if line == "DONE"
          then return ""
          else do
            more <- readResult
            return $ line ++ "\n" ++ more

    mutex <- newMVar ()

    return $ \formula inline -> unsafePerformIO $ do
        takeMVar mutex
        hPutStrLn stdinPipe formula
        hPutStrLn stdinPipe (if inline then "INLINE" else "NONINLINE")
        out <- readResult
        putMVar mutex ()
        return
            $ Text.replace " focusable=\"false\"" ""
            $ rmTrailingNewline -- Prevents artifacts in [rand.adapt.ibits]#4
            $ Text.pack
            $ rm " id=\"(MJXc|MathJax)-[0-9A-Za-z-]+\""
            $ rm " style=\"\""
            $ out

render :: Renderer
render = unsafePerformIO $ makeRenderer
