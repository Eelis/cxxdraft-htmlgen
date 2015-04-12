{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Util (
	mconcat, (.), (++), Text, replace, xml, spanTag, h,
	anchor, Anchor(..), writeFile
	) where

import Prelude hiding ((.), (++), writeFile)
import qualified Data.Text as Text
import Data.Text (Text, replace)
import Data.Monoid (Monoid(mappend), mconcat)
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

h :: Maybe Text -> Int -> Text -> Text
h mc = flip xml (maybe [] ((:[]) . ("class",)) mc) . ("h" ++) . Text.pack . show

data Anchor = Anchor { aClass, aId, aHref, aText :: Text }

anchor :: Anchor
anchor = Anchor{aClass="", aId="", aHref="", aText=""}
