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
import qualified Data.Map as Map
import Data.Map (Map)

rmTrailingNewline :: Text -> Text
rmTrailingNewline (Text.stripSuffix "\n" -> Just x) = x
rmTrailingNewline x = x

type Renderer = String {- formula -} -> Bool {- inline -} -> Text

data Input = Input { _formula :: String, _inline :: Bool }
    deriving (Eq, Ord)

makeRenderer :: IO Renderer
makeRenderer = do

    (Just stdinPipe, Just stdoutPipe, _, _) <- createProcess (shell "./mathjax-batch")
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

    mutex <- newMVar (Map.empty :: Map Input Text)

    return $ \formula inline -> unsafePerformIO $ do
        let input = Input formula inline
        cache <- takeMVar mutex
        (result, cache') <- case Map.lookup input cache of
            Just output -> do
                putStrLn "reusing"
                return (output, cache)
            Nothing -> do
                hPutStrLn stdinPipe formula
                hPutStrLn stdinPipe (if inline then "INLINE" else "NONINLINE")
                rawResult <- readResult
                let
                  output
                    = Text.replace " focusable=\"false\"" ""
                    $ rmTrailingNewline -- Prevents artifacts in [rand.adapt.ibits]#4
                    $ Text.pack
                    $ rm " id=\"(MJXc|MathJax)-[0-9A-Za-z-]+\""
                    $ rm " style=\"\""
                    $ rawResult
                return (output, Map.insert input output cache)
        putMVar mutex cache'
        return result

render :: Renderer
render = unsafePerformIO $ makeRenderer
