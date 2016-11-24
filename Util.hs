{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Util (
	mconcat, (.), (++), Text, replace, xml, spanTag, h,
	anchor, Anchor(..), writeFile, greekAlphabet, mapLast
	) where

import Prelude hiding ((.), (++), writeFile)
import qualified Data.Text as Text
import Data.Text (Text, replace)
import Data.Text.IO (writeFile)

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
