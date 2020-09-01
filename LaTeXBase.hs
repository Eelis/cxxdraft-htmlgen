{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module LaTeXBase
 ( MathType(..), LaTeXUnit(..), LaTeX, TeXArg, ArgKind(..), concatRaws, hasCommand, isJustRaw
 , matchCommand, lookForCommand, matchEnv, mapTeX, renderLaTeX, mapTeXRaw, isTeXEnv, texSpan, unconsRaw
 , trim, trimr, triml, texStripInfix, isCodeblock, isMath, texStripPrefix, texStripAnyPrefix, AllUnits(..) ) where

import Data.Monoid ((<>))
import Data.String (fromString)
import Prelude hiding ((.), (++), writeFile, dropWhile)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Char (isSpace)
import Util ((.), (++), textStripInfix)
import Control.Arrow (first, second)

data MathType = Parentheses | Square | Dollar
	deriving (Eq, Show, Ord)

data ArgKind = FixArg | OptArg
	deriving (Eq, Show, Ord)

type TeXArg = (ArgKind, LaTeX)

data LaTeXUnit
	= TeXRaw Text
	| TeXComm String String [TeXArg] -- first string is command name, second is trailing whitespace
	| TeXEnv String [TeXArg] LaTeX
	| TeXMath MathType LaTeX
	| TeXLineBreak
	| TeXBraces LaTeX
	deriving (Eq, Show, Ord)

isTeXEnv :: String -> LaTeXUnit -> Bool
isTeXEnv x (TeXEnv y _ _) = x == y
isTeXEnv _ _ = False

type LaTeX = [LaTeXUnit]

lookForCommand :: String -> LaTeX -> [[TeXArg]]
lookForCommand n = (snd .) . matchCommand (n ==)

class AllUnits a where
	allUnits :: a -> [LaTeXUnit]

instance AllUnits LaTeXUnit where
	allUnits u = u : case u of
		TeXMath _ l -> allUnits l
		TeXBraces l -> allUnits l
		TeXComm _ _ a -> (snd . a) >>= allUnits
		TeXEnv _ a l -> (l : snd . a) >>= allUnits
		_ -> []

instance AllUnits a => AllUnits [a] where
	allUnits = concatMap allUnits

matchCommand :: AllUnits a => (String -> Bool) -> a -> [(String, [TeXArg])]
matchCommand f x = [(str, as) | TeXComm str _ as <- allUnits x, f str]

hasCommand :: (String -> Bool) -> LaTeX -> Bool
hasCommand f = not . null . matchCommand f

matchEnv :: AllUnits a => (String -> Bool) -> a -> [(String, [TeXArg], LaTeX)]
matchEnv f x = [(str, as, l) | TeXEnv str as l <- allUnits x, f str]

mapTeX :: (LaTeXUnit -> Maybe LaTeX) -> LaTeX -> LaTeX
mapTeX f = concatMap g
	where
		g :: LaTeXUnit -> LaTeX
		g (f -> Just x) = x
		g (TeXComm c ws a) = [TeXComm c ws (h . a)]
		g (TeXBraces x) = [TeXBraces (mapTeX f x)]
		g (TeXMath t b) = [TeXMath t (mapTeX f b)]
		g (TeXEnv n a b) = [TeXEnv n (h . a) (mapTeX f b)]
		g x = [x]
		h = second (mapTeX f)

renderLaTeX :: LaTeX -> Text
renderLaTeX = mconcat . (renderUnit .)

renderUnit :: LaTeXUnit -> Text
renderUnit (TeXRaw t) = t
renderUnit (TeXComm "right" _ [(FixArg, [TeXRaw "."])]) = "\\right."
renderUnit (TeXComm name ws [])
	| name `elem` ["left", "sum", "int", "sin", "cos", "right", "bigl", "bigr", "big", "small", "smaller"] = pack $ "\\" <> name <> ws
	| otherwise = "\\" <> fromString name <> "{}"
renderUnit (TeXComm name ws args) = "\\" <> pack (fromString name) <> pack (fromString ws) <> renderArgs args
renderUnit (TeXEnv name args c) =
	"\\begin{" <> fromString name <> "}"
	<> renderArgs args
	<> renderLaTeX c
	<> "\\end{" <> fromString name <> "}"
renderUnit (TeXMath Dollar l) = "$" <> renderLaTeX l <> "$"
renderUnit (TeXMath Square l) = "\\[" <> renderLaTeX l <> "\\]"
renderUnit (TeXMath Parentheses l) = "\\(" <> renderLaTeX l <> "\\)"
renderUnit TeXLineBreak = "\\\\"
renderUnit (TeXBraces l) = "{" <> renderLaTeX l <> "}"

renderArgs :: [TeXArg] -> Text
renderArgs = mconcat . (renderArg .)

renderArg :: TeXArg -> Text
renderArg (FixArg, l) = "{" <> renderLaTeX l <> "}"
renderArg (OptArg, l) = "[" <> renderLaTeX l <> "]"

mapTeXRaw :: (Text -> LaTeXUnit) -> (LaTeX -> LaTeX)
mapTeXRaw f = map go
	where
		go :: LaTeXUnit -> LaTeXUnit
		go (TeXRaw t) = f t
		go (TeXComm s ws args) = TeXComm s ws (second (go .) . args)
		go (TeXEnv s args body) = TeXEnv s (second (go .) . args) (go . body)
		go (TeXBraces l) = TeXBraces $ go . l
		go t@(TeXMath _ _) = t
		go t@TeXLineBreak = t

concatRaws :: LaTeX -> LaTeX
concatRaws (TeXRaw a : TeXRaw b : more) = concatRaws (TeXRaw (a ++ b) : more)
concatRaws (TeXComm s ws args : more) = TeXComm s ws (second concatRaws . args) : concatRaws more
concatRaws (TeXEnv s args bd : more) = TeXEnv s (second concatRaws . args) (concatRaws bd) : concatRaws more
concatRaws (TeXBraces x : more) = TeXBraces (concatRaws x) : concatRaws more
concatRaws (x : more) = x : concatRaws more
concatRaws [] = []

unconsRaw :: LaTeX -> (Text, LaTeX)
unconsRaw (TeXRaw x : y) = first (x ++) (unconsRaw y)
unconsRaw x = ("", x)

texStripPrefix :: Text -> LaTeX -> Maybe LaTeX
texStripPrefix t (TeXRaw s : y) = case Text.stripPrefix t s of
	Just "" -> Just y
	Just s' -> Just (TeXRaw s' : y)
	Nothing -> Nothing
texStripPrefix _ _ = Nothing

texStripAnyPrefix :: [Text] -> LaTeX -> Maybe (Text, LaTeX)
texStripAnyPrefix [] _ = Nothing
texStripAnyPrefix (x:y) z
    | Just a <- texStripPrefix x z = Just (x, a)
    | otherwise = texStripAnyPrefix y z

texStripInfix :: Text -> LaTeX -> Maybe (LaTeX, LaTeX)
texStripInfix t = go
	where
		go [] = Nothing
		go (x : rest)
			| TeXRaw s <- x
			, Just (y, z) <- textStripInfix t s
				= Just (h y, h z ++ rest)
			| otherwise = first (x :) . go rest
		h "" = []
		h x = [TeXRaw x]

texSpan :: (Char -> Bool) -> LaTeX -> (Text, LaTeX)
texSpan p (TeXRaw x : y) = case Text.span p x of
    (stuff, "") ->  first (stuff ++) (texSpan p y)
    (stuff, rest) -> (stuff, TeXRaw rest : y)
texSpan _ x = ("", x)

invisible :: LaTeXUnit -> Bool
invisible (TeXComm "index" _ _) = True
invisible _ = False

dropWhileEnd :: (Char -> Bool) -> LaTeX -> LaTeX
dropWhileEnd _ [] = []
dropWhileEnd p x
	| invisible (last x) = dropWhileEnd p (init x) ++ [last x]
	| TeXRaw y <- last x = init x ++ case Text.dropWhileEnd p y of
		"" -> []
		a -> [TeXRaw a]
	| otherwise = x

trimr, trim :: LaTeX -> LaTeX
trimr = dropWhileEnd isSpace
trim = triml . trimr

triml :: LaTeX -> LaTeX
triml (TeXRaw x : y) = case Text.dropWhile isSpace x of
	"" -> triml y
	x' -> TeXRaw x' : y
triml x = x

isMath :: LaTeXUnit -> Bool
isMath (TeXMath _ _) = True
isMath (TeXComm "ensuremath" _ _) = True
isMath (TeXEnv "eqnarray*" _ _) = True
isMath _ = False

isCodeblock :: LaTeXUnit -> Bool
isCodeblock (TeXEnv "codeblock" _ _) = True
isCodeblock (TeXEnv "indexedcodeblock" _ _) = True
isCodeblock (TeXEnv "codeblocktu" _ _) = True
isCodeblock (TeXEnv "codeblockdigitsep" _ _) = True
isCodeblock _ = False

isJustRaw :: LaTeX -> Maybe Text
isJustRaw [TeXRaw x] = Just x
isJustRaw _ = Nothing
