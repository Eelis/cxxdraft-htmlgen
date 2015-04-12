{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Util (mconcat, (.), (++), Data.Text.Text, Text.replace, xml, spanTag, h) where

import qualified Data.Text as Text
import Data.Text (Text)
import Prelude hiding ((.), (++))
import Data.Monoid (Monoid(mappend), mconcat)

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

h :: Maybe Text -> Int -> Text -> Text
h mc = flip xml (maybe [] ((:[]) . ("class",)) mc) . ("h" ++) . Text.pack . show
