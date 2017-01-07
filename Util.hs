{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Util (
	mconcat, (.), (++), Text, replace, xml, spanTag, h, getDigit, startsWith,
	anchor, Anchor(..), writeFile, greekAlphabet, mapLast, mapHead, stripInfix
	) where

import Prelude hiding ((.), (++), writeFile)
import qualified Data.Text as Text
import Data.List (stripPrefix)
import Data.Char (ord, isDigit)
import Data.Text (Text, replace)
import Data.Text.IO (writeFile)
import Control.Arrow (first)

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

(++) :: Monoid a => a -> a -> a
(++) = mappend

xml :: Text -> [(Text, Text)] -> Text -> Text
xml t attrs = (("<" ++ t ++ " " ++ Text.unwords (map f attrs) ++ ">") ++) . (++ ("</" ++ t ++ ">"))
	where
		f (n, v) = n ++ "='" ++ v ++ "'"

spanTag :: Text -> Text -> Text
spanTag = xml "span" . (:[]) . ("class",)

h :: Int -> Text -> Text
h = flip xml [] . ("h" ++) . Text.pack . show

data Anchor = Anchor { aClass, aId, aHref, aText, aStyle :: Text }

anchor :: Anchor
anchor = Anchor{aClass="", aId="", aHref="", aText="", aStyle=""}

greekAlphabet :: [(String, Char)]
greekAlphabet =
	[ ("alpha"          , 'α')
	, ("beta"           , 'β')
	, ("delta"          , 'δ')
	, ("mu"             , 'μ')
	, ("nu"             , 'ν')
	, ("lambda"         , 'λ')
	, ("pi"             , 'π')
	, ("phi"            , 'φ')
	, ("rho"            , 'ρ')
	, ("sigma"          , 'σ')
	, ("theta"          , 'θ')
	, ("zeta"           , 'ζ')

	, ("Gamma"          , 'Γ')
	, ("Pi"             , 'Π') ]

mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = []
mapLast f [x] = [f x]
mapLast f (x:xx) = x : mapLast f xx

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:y) = f x : y
mapHead _ [] = []

getDigit :: Char -> Maybe Int
getDigit c
	| isDigit c = Just $ ord c - ord '0'
	| otherwise = Nothing

stripInfix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
stripInfix p s | Just r <- stripPrefix p s = Just ([], r)
stripInfix p (hd:t) = first (hd:) . stripInfix p t
stripInfix _ _  = Nothing

startsWith :: (Char -> Bool) -> (Text -> Bool)
startsWith _ "" = False
startsWith p t = p (Text.head t)
