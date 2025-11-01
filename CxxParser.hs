{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	TupleSections,
	ViewPatterns,
	LambdaCase,
	TypeSynonymInstances,
	FlexibleInstances #-}

module CxxParser (parseLiteral, parseComment, parseCppDirective) where

import LaTeXBase (LaTeX, LaTeXUnit(..), ArgKind(..), concatRaws,
    texStripPrefix, texStripAnyPrefix, texStripInfix, texSpan, unconsRaw)
import qualified Data.Text as Text
import Data.Char (isAlpha, isSpace, isAlphaNum, isDigit)
import Control.Arrow (first)
import Prelude hiding ((.), (++))
import Util ((.), (++), Text)

texStripHash :: LaTeX -> Maybe LaTeX
texStripHash x
    | Just x' <- texStripPrefix "#" x = Just x'
    | TeXComm "#" _ [] : x' <- x = Just x'
    | otherwise = Nothing

cppDirectives :: [Text]
cppDirectives = Text.words "include define elifndef elifdef ifndef endif ifdef pragma error undef line elif warning else if embed"

spanLiteralChars :: String -> (String, String {- rest without the closing ' -})
spanLiteralChars [] = ([], [])
spanLiteralChars ('\\' : '\'' : rest) = first ("\\'"++) (spanLiteralChars rest)
spanLiteralChars ('\\' : '\\' : rest) = first ("\\\\"++) (spanLiteralChars rest)
spanLiteralChars ('\'' : x) = ([], x)
spanLiteralChars (c : rest) = first (c :) (spanLiteralChars rest)

parseLiteralChars :: LaTeX -> (LaTeX, LaTeX)
parseLiteralChars [] = ([], [])
parseLiteralChars (TeXRaw s : rest) = case spanLiteralChars (Text.unpack s) of
    (x, []) -> first (TeXRaw (Text.pack x) :) (parseLiteralChars rest)
    (x, more) -> ([TeXRaw (Text.pack x)], TeXRaw (Text.pack more) : rest)
parseLiteralChars (x : rest) = first (x :) (parseLiteralChars rest)

parseCharLiteral :: LaTeX -> Maybe (LaTeX, LaTeX {- rest -})
parseCharLiteral x
    | Just (pre, x') <- texStripAnyPrefix ["'", "u'", "L'", "U'", "u8'"] x
    , (before, x'') <- parseLiteralChars x'
    , (suffix, x''') <- texSpan (\c -> isAlphaNum c || c == '_') x''
        = Just ([TeXRaw pre] ++ before ++ [TeXRaw $ "'" ++ suffix], x''')
    | otherwise = Nothing

parseCppDirective :: LaTeX -> Maybe (LaTeX, LaTeX {- rest -})
parseCppDirective x
    | Just x'' <- texStripHash x
    , (spaces, x''') <- texSpan isSpace x''
    , Just (directive, x'''') <- texStripAnyPrefix cppDirectives x'''
        = Just ([TeXRaw ("#" ++ spaces ++ directive)], x'''')
    | otherwise = Nothing

parseSingleLineComment :: LaTeX -> Maybe (LaTeX {- comment -}, LaTeX {- subsequent lines -})
parseSingleLineComment x
    | Just x' <- texStripPrefix "//" x = Just $ case texStripInfix "\n" x' of
        Just (commentLine, moreLines) -> (TeXRaw "//" : commentLine, TeXRaw "\n" : moreLines)
        Nothing -> (x, [])
    | rlap@(TeXComm "rlap" _ [(FixArg, [TeXComm "textnormal" _ [(FixArg,[TeXComm "textit" _ [(FixArg,[TeXRaw "//"])]])]])]) : more <- x
    , Just (commentLine, moreLines) <- texStripInfix "\n" more
        = Just ([rlap, TeXComm "tcode" "" [(FixArg, commentLine)]], TeXRaw "\n" : moreLines)
    | TeXComm "comment" _ [(FixArg, c)] : x' <- x = Just (c, x')
    | otherwise = Nothing

fromTeXRaw :: LaTeXUnit -> Text
fromTeXRaw (TeXRaw x) = x
fromTeXRaw x = error $ "fromTeXRaw (" ++ show x ++ ")"

parseStringLiteral :: LaTeX -> Maybe (LaTeX, LaTeX {- rest -})
parseStringLiteral x
    -- raw:
    | Just (pre, x') <- texStripAnyPrefix ["R\"", "u8R\"", "uR\"", "UR\"", "LR\""] x
    , Just (delim, x'') <- texStripInfix "(" x'
    , Just (body, x''') <- texStripInfix (")" ++ Text.concat (map fromTeXRaw delim) ++ "\"") (concatRaws $ f x'')
    , (suffix, x'''') <- texSpan (\c -> isAlphaNum c || c == '_') x'''
        = Just ([TeXRaw pre] ++ delim ++ [TeXRaw "("] ++ body ++ [TeXRaw ")"] ++ delim ++ [TeXRaw $ "\"" ++ suffix], x'''')
    -- normal:
    | Just (pre, x') <- texStripAnyPrefix ["\"", "u\"", "U\"", "L\"", "u8\""] x
    , Just (body, x'') <- parseBody x'
    , (suffix, x''') <- texSpan (\c -> isAlphaNum c || c == '_') x''
        = Just ([TeXRaw pre] ++ body ++ [TeXRaw $ "\"" ++ suffix], x''')
    | otherwise = Nothing
    where
        f :: LaTeX -> LaTeX
        f [] = []
        f (TeXComm "~" _ [] : more) = TeXRaw "~" : f more
        f (TeXBraces [] : more) = f more
        f (hd : t) = hd : f t
        parseBody :: LaTeX -> Maybe (LaTeX, LaTeX {- rest -})
        parseBody [] = Nothing
        parseBody (TeXComm "textbackslash" _ [] : more) = parseBody $ concatRaws $ TeXRaw "\\" : more
        parseBody (TeXRaw (Text.unpack -> raw) : more)
            | '\\':'"':t <- raw = first (TeXRaw "\\\"" :) . parseBody (TeXRaw (Text.pack t) : more)
            | "\"" <- raw = Just ([], more)
            | '"':t <- raw = Just ([], TeXRaw (Text.pack t) : more)
            | raw == "" = parseBody more
            | hd:t <- raw = first (TeXRaw (Text.pack [hd]) :) . parseBody (TeXRaw (Text.pack t) : more)
        parseBody (TeXComm "%" ws [] : more) = first (TeXComm "%" ws [] :) . parseBody more
        parseBody (y : more) = first (y :) . parseBody more

parseNumber :: LaTeX -> Maybe (Text, LaTeX)
parseNumber x
    | (raw, more) <- unconsRaw x
    , Just (n, rest) <- (parseStart `parseSeq` (\t -> Just (parseMany parseSuffix t))) raw
        = Just (n, TeXRaw rest : more)
    | otherwise = Nothing
    where
        parseDigit = parseChar isDigit
        parseNonDigit = parseChar (\c -> isAlpha c || c == '_')
        parseStart :: Text -> Maybe (Text, Text)
        parseStart = parseFirstOf [parseChar (== '.') `parseSeq` parseDigit, parseDigit]
        parseSign :: Text -> Maybe (Text, Text)
        parseSign = parseChar (\c -> c == '-' || c == '+')
        parseSuffix :: Text -> Maybe (Text, Text)
        parseSuffix = parseFirstOf
            [ parseDigit
            , parseChar (== '\'') `parseSeq` parseDigit
            , parseChar (== '\'') `parseSeq` parseNonDigit
            , parseChar (`elem` ("eEpP"::String)) `parseSeq` parseSign
            , parseChar (== '.')
            , parseNonDigit
            ]

parseLiteral :: LaTeX -> Maybe (LaTeX, LaTeX)
parseLiteral x
    | Just (number, x') <- parseNumber x = Just ([TeXRaw number], x')
    | Just (lit, x') <- parseCharLiteral x = Just (lit, x')
    | Just (lit, x') <- parseStringLiteral x = Just (lit, x')
    | otherwise = Nothing

parseComment :: LaTeX -> Maybe (LaTeX, LaTeX)
parseComment x
    | Just x' <- texStripPrefix "/*" x, Just (comment, x'') <- texStripInfix "*/" x'
        = Just ([TeXRaw "/*"] ++ comment ++ [TeXRaw "*/"], x'')
    | Just x' <- texStripPrefix "/*" x
        = Just ([TeXRaw "/*"], x')
    | Just x' <- texStripPrefix "*/" x
        = Just ([TeXRaw "*/"], x')
    | Just (comment, x') <- parseSingleLineComment x
        = Just (comment, x')
    | otherwise = Nothing

parseChar :: (Char -> Bool) -> Text -> Maybe (Text, Text)
parseChar p t
    | t /= "", p (Text.head t) = Just (Text.take 1 t, Text.drop 1 t)
    | otherwise = Nothing

parseSeq :: (Text -> Maybe (Text, Text)) -> (Text -> Maybe (Text, Text)) -> Text -> Maybe (Text, Text)
parseSeq p q t
    | Just (x, t') <- p t
    , Just (y, t'') <- q t' = Just (x ++ y, t'')
    | otherwise = Nothing

parseFirstOf :: [Text -> Maybe (a, Text)] -> Text -> Maybe (a, Text)
parseFirstOf [] _ = Nothing
parseFirstOf (p:pp) t
    | Just r <- p t = Just r
    | otherwise = parseFirstOf pp t

parseMany :: (Text -> Maybe (Text, Text)) -> Text -> (Text, Text)
parseMany p t = case p t of
    Nothing -> ("", t)
    Just (x, t') -> first (x++) (parseMany p t')
