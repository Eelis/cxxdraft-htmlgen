{-# LANGUAGE OverloadedStrings, ViewPatterns, LambdaCase #-}

module Sentences (splitIntoSentences, isActualSentence) where

import LaTeXBase (LaTeXUnit(..), triml, LaTeX, ArgKind(FixArg))
import Data.Text (isPrefixOf, isSuffixOf, stripPrefix)
import qualified Data.Text as Text
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (isSpace, isDigit, isUpper)
import Util ((++), textStripInfix, dropTrailingWs)
import RawDocument

startsSentence :: RawElement -> Bool
startsSentence (RawLatexElement e) | [TeXRaw x] <- triml [e], x /= "" = isUpper (Text.head x)
startsSentence _ = False

splitIntoSentences :: [RawElement] -> [[RawElement]]
splitIntoSentences = go []
	where
		go [] [] = []
		go [] (RawLatexElement (TeXRaw "\n") : y) = go [] y
		go [] (x@(RawExample _) : y) = [x] : go [] y
		go [] (x@(RawNote _ _) : y) = [x] : go [] y
		go partial (x@(RawCodeblock _) : y@(z : _)) | startsSentence z = (partial ++ [x]) : go [] y
		go x [] = [x]
		go x z@(e : y)
			| Just (s, rest) <- breakSentence z = (x ++ s) : go [] rest
			| otherwise = go (x ++ [e]) y

breakSentence :: [RawElement] -> Maybe ([RawElement] {- sentence -}, [RawElement] {- remainder -})
breakSentence (e@(RawLatexElement (TeXMath _ math)) : more)
    | f (reverse math) = Just ([e], more)
    where 
        f :: LaTeX -> Bool
        f (TeXRaw y : z) | all isSpace (Text.unpack y) = f z
        f (TeXComm "text" [(FixArg, a)] : _) = f (reverse a)
        f (TeXComm "mbox" [(FixArg, a)] : _) = f (reverse a)
        f (TeXRaw y : _) = "." `isSuffixOf` (Text.pack $ dropTrailingWs $ Text.unpack y)
        f _ = False
breakSentence (RawLatexElement (TeXRaw x) : more)
    | Just ((++ ".") -> pre, post) <- textStripInfix "." x = f pre post
  where
   f pre post
    | not (("(." `isSuffixOf` pre) && (")" `isPrefixOf` post))
    , not (("e." `isSuffixOf` pre) && ("g." `isPrefixOf` post))
    , not (("i." `isSuffixOf` pre) && ("e." `isPrefixOf` post))
    , not (Text.length pre > 1 && Text.length post > 0 && isDigit (Text.last $ Text.init pre) && isDigit (Text.head post))
    , not (("etc." `isSuffixOf` pre) && "," `isPrefixOf` post)
    , not ("e.g." `isSuffixOf` pre)
    , not ("i.e." `isSuffixOf` pre) =
        let
            post' = Text.dropWhile isSpace post
            (pre', post'') = case stripPrefix ")" post' of
                Just z -> (pre ++ ")" , Text.dropWhile isSpace z)
                Nothing -> (pre, post')
            more' = if post'' == "" then more else RawLatexElement (TeXRaw post'') : more
            (maybefootnote, more'') = case more' of
                fn@(RawLatexElement (TeXComm "footnoteref" _)) : z -> ([fn], z)
                _ -> ([], more')
            sentence = [RawLatexElement (TeXRaw pre')] ++ maybefootnote
        in
            Just (sentence, more'')
    | Just ((++ ".") -> pre', post') <- textStripInfix "." post = f (pre ++ pre') post'
    | otherwise = Nothing
breakSentence (enum@(RawEnumerated _ (last -> rawItemContent -> last -> RawTexPara (last -> el))) : more)
    | Just _ <- breakSentence [el] = Just ([enum], more)
breakSentence _ = Nothing

isActualSentence :: [RawElement] -> Bool
isActualSentence = any p
	where
		yes = words $
			"link tcode noncxxtcode textit ref grammarterm indexedspan " ++
			"defnx textbf textrm textsl textsc deflinkx"

		q :: LaTeXUnit -> Bool
		q (TeXRaw s) = not $ all isSpace $ Text.unpack s
		q (TeXComm c _) | c `elem` yes = True
		q (TeXEnv c _ _) | c `elem` yes = True
		q (TeXEnv "indexed" _ body) = any q body
		q (TeXBraces body) = any q body
		q _ = False

		p :: RawElement -> Bool
		p (RawLatexElement u) = q u
		p RawEnumerated{} = True
		p _ = False
