{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TupleSections #-}

module LaTeXUtil where

import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), texmap)
import Prelude hiding ((.), (++), writeFile, dropWhile)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Util
import Data.Maybe (isJust, fromJust)
import Data.Map (Map)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.Map as Map
import Util ((.), (++), getDigit, stripInfix)
import Control.Arrow (first)

texFromArg :: TeXArg -> LaTeX
texFromArg (FixArg t) = t
texFromArg (OptArg t) = t
texFromArg (SymArg t) = t
texFromArg _ = error "no"

mapTeXArg :: (LaTeX -> LaTeX) -> (TeXArg -> TeXArg)
mapTeXArg f (FixArg t) = FixArg (f t)
mapTeXArg f (OptArg t) = OptArg (f t)
mapTeXArg _ x = x

texTail :: LaTeX -> LaTeX
texTail (TeXSeq _ t) = t
texTail _ = error "Not a sequence"

mapTeXRaw :: (Text -> LaTeX) -> (LaTeX -> LaTeX)
mapTeXRaw f = go
	where
		go :: LaTeX -> LaTeX
		go (TeXRaw t) = f t
		go (TeXComm s args) = TeXComm s (mapTeXArg go . args)
		go (TeXEnv s args body) = TeXEnv s (mapTeXArg go . args) (go body)
		go (TeXBraces l) = TeXBraces $ go l
		go TeXEmpty = TeXEmpty
		go (TeXSeq x y) = TeXSeq (go x) (go y)
		go t@(TeXCommS _) = t
		go t@(TeXComment _) = t
		go t@(TeXMath _ _) = t
		go t@(TeXLineBreak _ _) = t

mapTeX :: (LaTeX -> Maybe LaTeX) -> (LaTeX -> LaTeX)
mapTeX f = texmap (isJust . f) (fromJust . f)

concatRaws :: LaTeX -> LaTeX
concatRaws l =
		let (a, b) =  go "" l
		in if b == "" then a else a ++ TeXRaw b
	where
		ppp :: Text -> (LaTeX -> LaTeX)
		ppp "" = id
		ppp t = TeXSeq (TeXRaw t)
		go :: Text -> LaTeX -> (LaTeX, Text)
		go pre t@TeXEmpty = (t, pre)
		go pre (TeXRaw s) = (TeXEmpty, pre ++ s)
		go pre (TeXEnv s args bd) = (ppp pre $ TeXEnv s (mapTeXArg concatRaws . args) (concatRaws bd), "")
		go pre (TeXComm s args) = (ppp pre $ TeXComm s (mapTeXArg concatRaws . args), "")
		go pre (TeXCommS s) = (ppp pre $ TeXCommS s, "")
		go pre (TeXBraces x) = (ppp pre $ TeXBraces (concatRaws x), "")
		go pre (TeXComment t) = (ppp pre $ TeXComment t, "")
		go pre (TeXMath m t) = (ppp pre $ TeXMath m t, "")
		go pre t@(TeXLineBreak _ _) = (ppp pre t, "")
		go pre (TeXSeq x y) =
			let
				(x', s) = go pre x
				(y', s') = go s y
			in
				(x' ++ y', s')

texText :: Text -> LaTeX
texText "" = TeXEmpty
texText x = TeXRaw x

texStripPrefix :: Text -> LaTeX -> Maybe LaTeX
texStripPrefix t = go
	where
		go (TeXRaw (Text.stripPrefix t -> Just rest))
			= Just (texText rest)
		go (TeXSeq x y)
			| Just x' <- go x = Just (x' ++ y)
		go _ = Nothing

texStripInfix :: Text -> LaTeX -> Maybe (LaTeX, LaTeX)
texStripInfix t = go
	where
		go (TeXRaw (unpack -> stripInfix (unpack t) -> Just ((pack -> x), (pack -> y))))
			= Just (texText x, texText y)
		go (TeXSeq x y)
			| Just (x', x'') <- go x = Just (x', x'' ++ y)
			| Just (y', y'') <- go y = Just (x ++ y', y'')
		go _ = Nothing

rmseqs :: LaTeX -> [LaTeX]
rmseqs (TeXSeq x y) = rmseqs x ++ rmseqs y
rmseqs x = [x]

dropWhile :: (Char -> Bool) -> LaTeX -> LaTeX
dropWhile p (TeXSeq x y) = TeXSeq (dropWhile p x) y
dropWhile p (TeXRaw x) = TeXRaw (Text.dropWhile p x)
dropWhile _ x = x

invisible :: LaTeX -> Bool
invisible (TeXComm "index" _) = True
invisible (TeXComment _) = True
invisible (TeXSeq x y) = invisible x && invisible y
invisible _ = False

dropWhileEnd :: (Char -> Bool) -> LaTeX -> LaTeX
dropWhileEnd p (TeXSeq x y)
	| invisible y = TeXSeq (dropWhileEnd p x) y
	| otherwise = TeXSeq x (dropWhileEnd p y)
dropWhileEnd p (TeXRaw x) = TeXRaw (Text.dropWhileEnd p x)
dropWhileEnd _ x = x

-- These dropWhile and dropWhileEnd only make a half-hearted effort, in that
-- they don't handle TeXRaws sequenced together, but we don't need that.

triml, trimr, trim :: LaTeX -> LaTeX
triml = dropWhile isSpace
trimr = dropWhileEnd isSpace
trim = triml . trimr

isMath :: LaTeX -> Bool
isMath (TeXMath _ _) = True
isMath (TeXComm "ensuremath" _) = True
isMath (TeXEnv "eqnarray*" _ _) = True
isMath _ = False

isCodeblock :: LaTeX -> Bool
isCodeblock (TeXEnv "codeblock" _ _) = True
isCodeblock (TeXEnv "codeblockdigitsep" _ _) = True
isCodeblock _ = False

needsSpace :: LaTeX -> Bool
	-- In the sense of \xspace
needsSpace TeXEmpty = False
needsSpace (TeXMath _ x) = needsSpace x
needsSpace (TeXSeq (TeXComm "index" _) x) = needsSpace x
needsSpace (TeXSeq x _) = needsSpace x
needsSpace (TeXComm "link" [FixArg x, _]) = needsSpace x
needsSpace (TeXComm "texttt" _) = True
needsSpace (TeXComm "mathsf" [FixArg x]) = needsSpace x
needsSpace (TeXComm "mathscr" [FixArg x]) = needsSpace x
needsSpace (TeXComm "tcode" [FixArg x]) = needsSpace x
needsSpace (TeXComm "textit" [FixArg x]) = needsSpace x
needsSpace (TeXComm "grammarterm_" _) = True
needsSpace (TeXComm "index" _) = False
needsSpace (TeXComm "sqrt" _) = True
needsSpace (TeXComm "ensuremath" (x : _)) = needsSpace $ texFromArg x
needsSpace (TeXComm "texorpdfstring" [_, FixArg x]) = needsSpace x
needsSpace (TeXCommS " ") = False
needsSpace (TeXCommS ",") = False
needsSpace (TeXCommS "~") = True
needsSpace (TeXCommS "&") = False
needsSpace (TeXBraces x) = needsSpace x
needsSpace (TeXRaw t) = Util.startsWith (\c -> isAlphaNum c || (c `elem` ("~&-!*(" :: String))) t
needsSpace x = error $ "needsSpace: unexpected: " ++ show x
