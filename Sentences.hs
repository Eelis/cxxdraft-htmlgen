{-# LANGUAGE OverloadedStrings, ViewPatterns, LambdaCase, TypeSynonymInstances, FlexibleInstances #-}

module Sentences (splitIntoSentences, isActualSentence, linkifyFullStop, breakSentence) where

import LaTeXBase (LaTeXUnit(..), triml, LaTeX, ArgKind(FixArg))
import Data.Text (isPrefixOf, isSuffixOf, stripPrefix, Text)
import qualified Data.Text as Text
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (isSpace, isDigit, isAlphaNum, isUpper, isLower)
import Control.Arrow (first)
import Data.Maybe (isNothing)
import Util ((++), textStripInfix, dropTrailingWs, (.))
import RawDocument
import Document

startsSentence :: RawElement -> Bool
startsSentence (RawLatexElement e) | [TeXRaw x] <- triml [e], x /= "" = isUpper (Text.head x)
startsSentence _ = False

unitContinuesSentence :: LaTeXUnit -> Bool
unitContinuesSentence (TeXComm " " _ []) = True
unitContinuesSentence (TeXRaw txt) = "," `isPrefixOf` txt
unitContinuesSentence _ = False

elemContinuesSentence :: RawElement -> Bool
elemContinuesSentence (RawLatexElement u) = unitContinuesSentence u
elemContinuesSentence _ = False

elemsContinueSentence :: [RawElement] -> Bool
elemsContinueSentence (RawLatexElement (TeXRaw "") : more) = elemsContinueSentence more
elemsContinueSentence (x : _) = elemContinuesSentence x
elemsContinueSentence _ = False

simpleHead :: [RawElement] -> Maybe Char
simpleHead [] = Nothing
simpleHead (RawLatexElement (TeXRaw x) : more)
	| x == "" = simpleHead more
	| otherwise = Just (Text.head x)
simpleHead (RawLatexElement (TeXComm " " "" []) : _) = Just ' '
simpleHead (RawLatexElement (TeXComm "tcode" _ [(_, x)]) : more) = simpleHead (map RawLatexElement x ++ more)
simpleHead (RawLatexElement (TeXComm "index" _ _) : more) = simpleHead more
simpleHead (RawLatexElement (TeXComm "footnoteref" _ _) : _) = Nothing -- hmm
simpleHead (RawLatexElement TeXLineBreak : _) = Nothing
simpleHead (RawLatexElement (TeXComm "br" _ _) : _) = Nothing
simpleHead (RawLatexElement (TeXComm "newline" _ _) : _) = Nothing
simpleHead (RawLatexElement (TeXComm "par" _ _) : _) = Nothing
simpleHead (RawLatexElement (TeXComm "nolinebreak" _ _) : _) = Nothing
simpleHead (RawLatexElement (TeXComm "iref" _ _) : _) = Nothing
simpleHead x = error $ "simpleHead: " ++ show x

splitIntoSentences :: [RawElement] -> [[RawElement]]
splitIntoSentences = go []
	where
		go [] [] = []
		go [] (RawLatexElement (TeXRaw "\n") : y) = go [] y
		go [] (x@(RawExample _) : y) = [x] : go [] y
		go [] (x@(RawNote _ _) : y) = [x] : go [] y
		go partial (x@(RawCodeblock _) : y) | z : _ <- rmIndices y, startsSentence z = (partial ++ [x]) : go [] y
		go x [] = [x]
		go x z@(e : y)
			| Just (s, rest) <- breakSentence z = (x ++ s) : go [] rest
			| otherwise = go (x ++ [e]) y
		rmIndices (RawLatexElement (TeXRaw "\n") : RawLatexElement (TeXComm "index" _ _) : x) = rmIndices x
		rmIndices x = x

breakSentence :: [RawElement] -> Maybe ([RawElement] {- sentence -}, [RawElement] {- remainder -})
breakSentence (e@(RawLatexElement (TeXMath _ math)) : more)
    | f (reverse math) = Just ([e], more)
    | otherwise = first (e :) . breakSentence more
    where
        f :: LaTeX -> Bool
        f (TeXRaw y : z) | all isSpace (Text.unpack y) = f z
        f (TeXComm "text" _ [(FixArg, a)] : _) = f (reverse a)
        f (TeXComm "mbox" _ [(FixArg, a)] : _) = f (reverse a)
        f (TeXRaw ".\n" : TeXComm "right" "" [] : y) = f y
        f (TeXRaw y : _) = "." `isSuffixOf` (Text.pack $ dropTrailingWs $ Text.unpack y)
        f _ = False
breakSentence (b@(RawLatexElement TeXLineBreak) : more) = Just ([b], more)
breakSentence (RawLatexElement (TeXBraces x) : more) = breakSentence (map RawLatexElement x ++ more)
breakSentence (e@(RawLatexElement (TeXEnv "eqnarray*" _ _)) : more) = first (e :) . breakSentence more
breakSentence (b@(RawLatexElement (TeXComm cmd _ _)) : more) =
	if cmd `elem` ["break"]
		then Just ([b], more)
		else (first (b :)) . breakSentence more
breakSentence (e@(RawLatexElement (TeXRaw (textStripInfix "." -> (Just ((++ ".") -> pr, po))))) : more)
    = f pr po
  where
   f :: Text -> Text -> Maybe ([RawElement], [RawElement])
   f pre post
    | "”" `isPrefixOf` post = f (pre ++ "”") (Text.drop 1 post)
    | not (("(." `isSuffixOf` pre) && (")" `isPrefixOf` post))
    , not ("" == post && maybe False (\c -> isLower c || isDigit c) (simpleHead more))
    , not ("" == post && length more /= 0 && head more == RawLatexElement (TeXComm " " "" []))
    , not (Text.length post > 0 && ((Text.head post == '.')
                                    || isLower (Text.head post)
                                    || isDigit (Text.head post)))
    , not (Text.length pre > 1 && Text.length post > 0 && isAlphaNum (Text.last $ Text.init pre) && isDigit (Text.head post))
    , not (elemsContinueSentence (RawLatexElement (TeXRaw post) : more))
    , not (Text.length pre >= 2 && ("." `isSuffixOf` pre) && isUpper (Text.last $ Text.init pre))
    , not ("e.g." `isSuffixOf` pre)
    , not ("i.e." `isSuffixOf` pre) =
        let
            post' = Text.stripStart post
            (pre', post'') = case stripPrefix ")" post' of
                Just z -> (pre ++ ")" , Text.stripStart z)
                Nothing -> (pre, post')
            more' = if post'' == "" then more else RawLatexElement (TeXRaw post'') : more
            (maybefootnote, more'') = case more' of
                fn@(RawLatexElement (TeXComm "footnoteref" _ _)) : z -> ([fn], z)
                _ -> ([], more')
            sentence = [RawLatexElement (TeXRaw pre')] ++ maybefootnote
        in
            Just (sentence, more'')
    | Just ((++ ".") -> pre', post') <- textStripInfix "." post = f (pre ++ pre') post'
    | otherwise = first (e :) . breakSentence more
breakSentence (e@(RawLatexElement (TeXRaw _)) : more) = first (e :) . breakSentence more
breakSentence (enum@(RawEnumerated _ (last -> rawItemContent -> (_ : _ : _))) : more)
    = Just ([enum], more)
breakSentence (enum@(RawEnumerated _ (last -> rawItemContent -> [RawTexPara y])) : more)
    | Just _ <- breakSentence y = Just ([enum], more)
breakSentence _ = Nothing

isActualSentence :: [RawElement] -> Bool
isActualSentence (RawEnumerated _ _ : _) = False
isActualSentence l = any p l
	where
		yes = words $
			"link tcode noncxxtcode textit ref grammarterm indexedspan " ++
			"defnx textbf textrm textsl textsc indexlink hiddenindexlink"

		q :: LaTeXUnit -> Bool
		q (TeXRaw s) = not $ all isSpace $ Text.unpack s
		q (TeXComm c _ _) | c `elem` yes = True
		q (TeXEnv c _ _) | c `elem` yes = True
		q (TeXEnv "indexed" _ body) = any q body
		q (TeXBraces body) = any q body
		q _ = False

		p :: RawElement -> Bool
		p (RawLatexElement u) = q u
		p RawEnumerated{} = True
		p _ = False

class LinkifyFullStop a where
    linkifyFullStop :: LaTeXUnit -> a -> Maybe a

instance LinkifyFullStop LaTeX where
    linkifyFullStop link l = reverse . f (reverse l)
      where
        f [] = Nothing
        f (x@(TeXRaw ".\n") : y@(TeXComm "right" _ _) : more) = ([x, y] ++) . f more
        f (u : uu)
            | Just u' <- inUnit u = Just (reverse u' ++ uu)
            | otherwise = (u :) . f uu
        inUnit :: LaTeXUnit -> Maybe LaTeX -- returns content in regular order
        inUnit (TeXEnv "array" args body)
            | Just body' <- linkifyFullStop link body = Just [TeXEnv "array" args body']
        inUnit (TeXEnv "indented" [] body)
            | Just body' <- linkifyFullStop link body = Just [TeXEnv "indented" [] body']
        inUnit (TeXComm "text" ws [(FixArg, x)])
            | Just x' <- linkifyFullStop link x = Just (moveStuffOutsideText (TeXComm "text" ws [(FixArg, x')]))
            | otherwise = Nothing
        inUnit (TeXComm "mbox" ws [(FixArg, x)])
            | Just x' <- linkifyFullStop link x = Just (moveStuffOutsideText (TeXComm "mbox" ws [(FixArg, x')]))
            | otherwise = Nothing
        inUnit (TeXMath kind m)
            | Just m' <- linkifyFullStop link m = Just [TeXMath kind m']
        inUnit (TeXRaw (Text.dropWhileEnd (=='\n') -> Text.stripSuffix "." -> Just s)) = Just [TeXRaw s, link]
        inUnit (TeXRaw (Text.stripSuffix ".)" -> Just s)) = Just [TeXRaw s, link, TeXRaw ")"]
        inUnit (TeXRaw (Text.stripSuffix ".”" -> Just s)) = Just [TeXRaw s, link, TeXRaw "”"]
        inUnit _ = Nothing

instance LinkifyFullStop Item where
    linkifyFullStop link it@Item{itemInlineContent=e}
        | Just y <- linkifyFullStop link e
            = Just it{itemInlineContent=y}
    linkifyFullStop _ _ = Nothing

instance LinkifyFullStop [Element] where
    linkifyFullStop link = (reverse .) . f . reverse
      where
        f :: [Element] -> Maybe [Element]
        f (Enumerated cmd (reverse -> (lastItem : moreItems)) : more)
            | all (isNothing . linkifyFullStop link) moreItems
            , Just lastItem' <- linkifyFullStop link lastItem
            = Just $ Enumerated cmd (reverse (lastItem' : moreItems)) : more
        f (LatexElement u : more)
            | Just u' <- linkifyFullStop link [u] = Just $ map LatexElement (reverse u') ++ more
            | otherwise = (LatexElement u :) . f more
        f _ = Nothing

moveStuffOutsideText :: LaTeXUnit -> LaTeX
    -- Turns \text{ \class{bla} } into \text{ }\class{\text{bla}}\text{ }, and similar for \href,
    -- because MathJax does not support \class and \href in \text.
moveStuffOutsideText (TeXComm parent pws [(FixArg, [TeXComm nested nws [x, y]])])
    | parent `elem` ["text", "mbox"]
    , nested `elem` ["class", "href"] = [TeXComm nested nws [x, (FixArg, moveStuffOutsideText (TeXComm parent pws [y]))]]
moveStuffOutsideText (TeXComm parent pws [(FixArg, t)])
    | parent `elem` ["text", "mbox"]
    , length t >= 2 = concatMap (\u -> moveStuffOutsideText $ TeXComm parent pws [(FixArg, [u])]) t
moveStuffOutsideText u = [u]
