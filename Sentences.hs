{-# LANGUAGE OverloadedStrings, ViewPatterns, LambdaCase, TypeSynonymInstances, FlexibleInstances #-}

module Sentences (splitIntoSentences, isActualSentence, linkifyFullStop) where

import LaTeXBase (LaTeXUnit(..), triml, LaTeX, ArgKind(FixArg))
import Data.Text (isPrefixOf, isSuffixOf, stripPrefix, Text)
import qualified Data.Text as Text
import Prelude hiding (take, (.), takeWhile, (++), lookup, readFile)
import Data.Char (isSpace, isDigit, isAlphaNum, isUpper, isLower)
import Control.Arrow (first)
import Data.Maybe (isNothing)
import Util ((++), textStripInfix, dropTrailingWs, (.), trimString)
import RawDocument
import Document

startsSentence :: RawElement -> Bool
startsSentence (RawLatexElement e) | [TeXRaw x] <- triml [e], x /= "" = isUpper (Text.head x)
startsSentence _ = False

unitContinuesSentence :: LaTeXUnit -> Bool
unitContinuesSentence (TeXComm " " []) = True
unitContinuesSentence (TeXRaw txt) = "," `isPrefixOf` txt
unitContinuesSentence _ = False

elemContinuesSentence :: RawElement -> Bool
elemContinuesSentence (RawLatexElement u) = unitContinuesSentence u
elemContinuesSentence _ = False

elemsContinueSentence :: [RawElement] -> Bool
elemsContinueSentence (RawLatexElement (TeXRaw "") : more) = elemsContinueSentence more
elemsContinueSentence (x : _) = elemContinuesSentence x
elemsContinueSentence _ = False

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
		rmIndices (RawLatexElement (TeXRaw "\n") : RawLatexElement (TeXComm "index" _) : x) = rmIndices x
		rmIndices x = x

breakSentence :: [RawElement] -> Maybe ([RawElement] {- sentence -}, [RawElement] {- remainder -})
breakSentence (e@(RawLatexElement (TeXMath _ math)) : more)
    | f (reverse math) = Just ([e], more)
    | otherwise = first (e :) . breakSentence more
    where
        f :: LaTeX -> Bool
        f (TeXRaw y : z) | all isSpace (Text.unpack y) = f z
        f (TeXComm "text" [(FixArg, a)] : _) = f (reverse a)
        f (TeXComm "mbox" [(FixArg, a)] : _) = f (reverse a)
        f (TeXRaw y : _) = "." `isSuffixOf` (Text.pack $ dropTrailingWs $ Text.unpack y)
        f _ = False
breakSentence (b@(RawLatexElement TeXLineBreak) : more) = Just ([b], more)
breakSentence (RawLatexElement (TeXBraces x) : more) = breakSentence (map RawLatexElement x ++ more)
breakSentence (b@(RawLatexElement (TeXComm cmd _)) : more) =
	if cmd `elem` ["break"]
		then Just ([b], more)
		else (first (b :)) . breakSentence more
breakSentence (RawLatexElement (TeXRaw (textStripInfix "." -> (Just ((++ ".") -> pre, post)))) : more)
    = f pre post
  where
   f pre post
    | not (("(." `isSuffixOf` pre) && (")" `isPrefixOf` post))
    , not (Text.length post > 0 && isLower (Text.head post))
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
                fn@(RawLatexElement (TeXComm "footnoteref" _)) : z -> ([fn], z)
                _ -> ([], more')
            sentence = [RawLatexElement (TeXRaw pre')] ++ maybefootnote
        in
            Just (sentence, more'')
    | Just ((++ ".") -> pre', post') <- textStripInfix "." post = f (pre ++ pre') post'
    | otherwise = Nothing
breakSentence (e@(RawLatexElement (TeXRaw _)) : more) = first (e :) . breakSentence more
breakSentence (enum@(RawEnumerated _ (last -> rawItemContent -> (_ : _ : _))) : more)
    = Just ([enum], more)
breakSentence (enum@(RawEnumerated x (last -> rawItemContent -> [RawTexPara y])) : more)
    | Just _ <- breakSentence y = Just ([enum], more)
breakSentence _ = Nothing

isActualSentence :: [RawElement] -> Bool
isActualSentence (RawEnumerated _ _ : _) = False
isActualSentence l = any p l
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

class LinkifyFullStop a where
    linkifyFullStop :: LaTeXUnit -> a -> Maybe a

instance LinkifyFullStop LaTeX where
    linkifyFullStop link l = reverse . f (reverse l)
      where
        f [] = Nothing
        f (u : uu)
            | Just u' <- inUnit u = Just (reverse u' ++ uu)
            | otherwise = (u :) . f uu
        inUnit :: LaTeXUnit -> Maybe LaTeX -- returns content in regular order
        inUnit (TeXEnv "array" args body)
            | Just body' <- linkifyFullStop link body = Just [TeXEnv "array" args body']
        inUnit (TeXEnv "indented" [] body)
            | Just body' <- linkifyFullStop link body = Just [TeXEnv "indented" [] body']
        inUnit (TeXComm "text" [(FixArg, x)])
            | Just x' <- linkifyFullStop link x = Just (moveStuffOutsideText (TeXComm "text" [(FixArg, x')]))
            | otherwise = Nothing
        inUnit (TeXComm "mbox" [(FixArg, x)])
            | Just x' <- linkifyFullStop link x = Just (moveStuffOutsideText (TeXComm "mbox" [(FixArg, x')]))
            | otherwise = Nothing
        inUnit (TeXMath kind m)
            | Just m' <- linkifyFullStop link m = Just [TeXMath kind m']
        inUnit (TeXRaw (Text.dropWhileEnd (=='\n') -> Text.stripSuffix "." -> Just s)) = Just [TeXRaw s, link]
        inUnit (TeXRaw (Text.stripSuffix ".)" -> Just s)) = Just [TeXRaw s, link, TeXRaw ")"]
        inUnit _ = Nothing

instance LinkifyFullStop Item where
    linkifyFullStop link it@Item{itemContent=[TeXPara (s@Sentence{sentenceElems=e} : ss)]}
        | (x:_) <- Text.unpack (Text.dropWhile isSpace (justText e))
        , isLower x
        , Just y <- linkifyFullStop link e
            = Just it{itemContent=[TeXPara (s{sentenceElems = y} : ss)]}
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
moveStuffOutsideText (TeXComm parent [(FixArg, [TeXComm nested [x, y]])])
    | parent `elem` ["text", "mbox"]
    , nested `elem` ["class", "href"] = [TeXComm nested [x, (FixArg, moveStuffOutsideText (TeXComm parent [y]))]]
moveStuffOutsideText (TeXComm parent [(FixArg, t)])
    | parent `elem` ["text", "mbox"]
    , length t >= 2 = concatMap (\u -> moveStuffOutsideText $ TeXComm parent [(FixArg, [u])]) t
moveStuffOutsideText u = [u]

justText :: [Element] -> Text
justText = Text.concat . map g
    where
        g :: Element -> Text
        g = ff . elemTex
        ff :: LaTeX -> Text
        ff = Text.concat . map f
        f :: LaTeXUnit -> Text
        f (TeXRaw x) = x
        f TeXLineBreak = ""
        f (TeXEnv "codeblock" _ _) = "code"
        f (TeXBraces x) = ff x
        f (TeXMath _ x) = ff x
        f (TeXComm " " _) = ""
        f (TeXComm c _)
            | trimString c `elem` words ("left right dotsc cdots lceil rceil lfloor rfloor cdot phantom index & { } # @ - ; " ++
                                         "linebreak ~ textunderscore leq geq lfloor rfloor footnoteref discretionary nolinebreak setlength") = ""
            | c == "ref" = "bla"
            | c == "nolinebreak" = ""
        f (TeXComm c ((FixArg, x) : _))
            | trimString c `elem` words "deflinkx link liblinkx tcode noncxxtcode textsc mathscr term mathsf mathit text textit texttt mathtt grammarterm ensuremath textsc mathbin url" = ff x
        f (TeXComm c (_ : (FixArg, x) : _))
            | c `elem` ["defnx", "grammarterm_"] = ff x
        f _ = {-trace ("justText: " ++ show x)-} ""
