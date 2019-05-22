{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TupleSections #-}

module LaTeXParser (parseString, Context(..), defaultContext, Signature(..), Macros(..), Command(..), Environment(..)) where

import LaTeXBase (LaTeXUnit(..), LaTeX, TeXArg, ArgKind(..), MathType(..), concatRaws)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isAlphaNum, isSpace, isAlpha, isDigit)
import Data.Maybe (fromJust)
import Control.Arrow (first)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding ((++), (.))
import Util ((.), (++), getDigit, stripInfix)

newtype Token = Token { tokenChars :: String }
	deriving (Eq, Show)

data Environment = Environment
	{ envSignature :: Signature
	, begin, end :: ![Token] }
	deriving Show

data Macros = Macros
	{ commands :: Map String Command
	, environments :: Map Text Environment
	, counters :: Map Text Int }
	deriving Show

instance Monoid Macros where
	mempty = Macros mempty mempty mempty
	mappend x y = Macros
		(commands x ++ commands y)
		(environments x ++ environments y)
		(counters x ++ counters y)

data Command = Command
	{ cmdSignature :: Signature
	, cmdBody :: ![Token] }
	deriving Show

data ParseResult = ParseResult
	{ content :: LaTeX
	, newMacros :: Macros
	, remainder :: [Token] }

data Signature = Signature
	{ nrFixArgs :: Int
	, defaultArg :: Maybe [Token] }
	deriving Show

data Context = Context
	{ commentsEnabled :: Bool
	, parsingOptArg :: Bool
	, macros :: Macros
	, signatures :: [(String, Signature)]
	, kill :: [String]
	, dontEval :: [String] }

prependContent :: LaTeX -> ParseResult -> ParseResult
prependContent t p = p{content = t ++ content p}

addMacros :: Macros -> ParseResult -> ParseResult
addMacros m p = p{newMacros = m ++ newMacros p}

defaultContext :: Context
defaultContext = Context
	{ commentsEnabled = True
	, parsingOptArg = False
	, macros = mempty
	, signatures = []
	, kill = []
	, dontEval = [] }

rmLine :: [Token] -> [Token]
rmLine s = case dropWhile (/= Token "\n") s of
	Token "\n" : x -> x
	x -> x

makeEnv :: [(String, Signature)]
makeEnv =
	[ ("importgraphic", Signature 3 Nothing)
	, ("minipage", Signature 1 Nothing)
	, ("tabular", Signature 1 Nothing)
	, ("array", Signature 1 Nothing)
	, ("TableBase", Signature 1 Nothing)
	, ("lib2dtab2", Signature 4 Nothing)
	, ("note", Signature 0 (Just [Token "Note"]))
	, ("longtable", Signature 1 Nothing)
	, ("indexed", Signature 1 Nothing)
	, ("itemdecl", Signature 0 Nothing) ] -- todo: move

parseOptArg :: [Token] -> Maybe ([Token], [Token])
parseOptArg = balanced ('[', ']')

parseOptArgs :: [Token] -> ([[Token]], [Token])
parseOptArgs s
	| Just (r, s') <- parseOptArg s = first (r:) (parseOptArgs s')
	| otherwise = ([], s)

parseFixArg :: Context -> [Token] -> Maybe (LaTeX, [Token])
parseFixArg ctx (Token [c] : more) | isSpace c = parseFixArg ctx more
parseFixArg ctx (Token "{" : more) =
	let ParseResult t _macros s = parse ctx more in Just (t, s)
parseFixArg _ _ = Nothing

parseSignature :: [Token] -> (Signature, [Token])
parseSignature t = case optArgs of
	[] -> (Signature 0 Nothing, t')
	[[Token a]] -> (Signature (read a) Nothing, t')
	[[Token a], deflt] -> (Signature (read a) (Just deflt), t')
	_ -> error "unrecognized signature"
	where (optArgs, t') = parseOptArgs t

parseNewCmd :: Context -> [Token] -> ParseResult
parseNewCmd c@Context{..} (Token ('\\' : name) : Token "}" : rest) =
	let
		(sig, rest') = parseSignature rest
		Just (body, rest'') = balanced ('{', '}') rest'
		m = Macros (Map.singleton name (Command sig body)) mempty mempty
		ParseResult p mm r = parse c{macros = macros ++ m} rest''
	in
		ParseResult p (m ++ mm) r
parseNewCmd _ x = error $ "parseNewCmd: unexpected: " ++ take 100 (show x)

balanced :: (Char, Char) -> [Token] -> Maybe ([Token], [Token])
balanced (open, close) (dropWhile (all isSpace . tokenChars) -> (Token [o] : s))
	| o == open = Just $ go 0 s
	where
		go :: Int -> [Token] -> ([Token], [Token])
		go 0 [] = ([], [])
		go 0 (Token [c] : x) | c == close = ([], x)
		go n (Token "}" : x) = first (Token "}" :) (go (n-1) x)
		go n (Token "{" : x) = first (Token "{" :) (go (n+1) x)
		go n (x:y) = first (x :) (go n y)
		go n x = error $ "\n\nbalanced: " ++ show (n, x)
balanced oc (dropWhile (all isSpace. tokenChars) -> (Token "%" : x)) = balanced oc (dropWhile (/= Token "\n") x)
balanced _ _ = Nothing

balanced_body :: String -> [Token] -> ([Token], [Token])
balanced_body end = go 0
	where
		go :: Int -> [Token] -> ([Token], [Token])
		go 0 [] = ([], [])
		go 0 (Token "\\end" : Token "{" : Token e : Token "}" : x) | e == end = ([], x)
		go n (Token "}" : x) = first (Token "}" :) (go (n-1) x)
		go n (Token "{" : x) = first (Token "{" :) (go (n+1) x)
		go n (x:y) = first (x :) (go n y)
		go n s = error $ "\n\nbalanced: " ++ show (n, s)

parseArgs :: Signature -> [Token] -> ([[Token]], [Token])
parseArgs Signature{..} s = case defaultArg of
	Nothing -> n_balanced ('{', '}') nrFixArgs s
	Just dfl -> case parseOptArg s of
		Nothing ->
			first (dfl :) (n_balanced ('{', '}') (nrFixArgs - 1) s)
		Just (optArg, s') ->
			first (optArg :) (n_balanced ('{', '}') (nrFixArgs - 1) s')

parseArgs2 :: Context -> Signature -> [Token] -> ([TeXArg], [Token])
parseArgs2 c Signature{..} s
	| defaultArg == Nothing = first (map fa) (n_balanced ('{', '}') nrFixArgs s)
	| Just (optArg, s') <- parseOptArg s =
			first (\a -> (OptArg, fullParse c optArg) : map fa a)
			(n_balanced ('{', '}') (nrFixArgs - 1) s')
	| otherwise = first (map fa) (n_balanced ('{', '}') (nrFixArgs - 1) s)
	where
		fa = (FixArg, ) . fullParse c

-- todo: clean up parseArgs/parseArgs2 above

n_balanced :: (Char, Char) -> Int -> [Token] -> ([[Token]], [Token])
n_balanced oc n s
	| n > 0, Just (x, s') <- balanced oc s = first (x:) $ n_balanced oc (n-1) s'
	| otherwise = ([], s)

parseNewEnv :: Context -> [Token] -> ParseResult
parseNewEnv c@Context{..} s =
	let
		(name, Token "}" : rest) = span (/= Token "}") s
		(sig, rest') = parseSignature rest
		Just (begin, rest'') = balanced ('{', '}') rest'
		Just (end, rest''') = balanced ('{', '}') rest''
		env = Environment sig begin end
		m = Macros mempty (Map.singleton (Text.pack $ concatMap tokenChars name) env) mempty
	in
		addMacros m (parse c{macros=macros++m} rest''')

parseString :: Context -> String -> (LaTeX, Macros, [Token])
parseString c s = (concatRaws x, y, z)
	where ParseResult x y z = parse c (tokenize s)

literal :: String
literal = " @_{}&,%-#/~>!$;:^"

parseCode :: Context -> [Token] -> LaTeX
parseCode c = concatRaws . go False
	where
		go :: Bool {- in string literal -} -> [Token] -> LaTeX
		go _ [] = []
		go b (Token "@" : rest) = fullParse c cmd ++ go b rest'
			where (cmd, Token "@" : rest') = break (== Token "@") rest
		go True (Token "\"" : rest) = TeXRaw "\"" : go False rest
		go False (Token "\"" : rest) = TeXRaw "\"" : (go True lit ++ go False rest')
			where (lit, rest') = stringLiteral rest
		go False (Token "/" : Token "/" : (break (== Token "\n") -> (comment, rest')))
			= TeXComm "comment" [(FixArg, TeXRaw "//" : noncode comment)] : go False rest'
		go False (Token "/" : Token "*" : rest)
		    | Just (comment, rest') <- stripInfix [Token "*", Token "/"] rest
		    = TeXComm "comment" [(FixArg, [TeXRaw "/*"] ++ noncode comment ++ [TeXRaw "*/"])] : go False rest'
		go b (Token "/" : rest) = TeXRaw "/" : go b rest
		go b s = TeXRaw (Text.pack $ concatMap tokenChars code) : go b rest
			where (code, rest) = break (`elem` [Token "@", Token "/", Token "\""]) s
		noncode :: [Token] -> LaTeX
		noncode toks =
		  fullParse c nc ++ case more of
		    [] -> []
		    Token "@" : (break (== Token "@") -> (code, _ : rest)) ->
		        TeXComm "tcode" [(FixArg, fullParse c code)] : noncode rest
		    _ -> error "no"
		    where (nc, more) = span (/= Token "@") toks
		stringLiteral :: [Token] -> ([Token], [Token])
		stringLiteral (Token "\\" : Token "\"" : x) = first (Token "\\\"" :) (stringLiteral x)
		stringLiteral (Token "\\" : Token "\\" : x) = first (Token "\\\\" :) (stringLiteral x)
		stringLiteral (Token "\"" : x) = ([Token "\""], x)
		stringLiteral (y : x) = first (y :) (stringLiteral x)
		stringLiteral [] = ([], [])

tokenize :: String -> [Token]
tokenize "" = []
tokenize ('\\':'v':'e':'r':'b': delim : (break (== delim) -> (arg, _ : rest))) =
	Token ("\\verb:" ++ arg) : tokenize rest
tokenize ('\\' : (span isAlpha -> (cmd@(_:_), (span (== ' ') -> (ws, rest)))))
	= Token ('\\' : cmd ++ ws) : tokenize rest
tokenize ('\\' : c : rest) = Token ['\\', c] : tokenize rest
tokenize x@((isAlpha -> True): _) = let (a, b) = span isAlphaNum x in Token a : tokenize b
tokenize (x:y) = Token [x] : tokenize y

	-- \verb is handled in tokenize so that the 'balanced' function doesn't
	-- get confused by \verb|{|

	-- Notice how the whitespace following a command like \bla is included in the Token
	-- This lets the parser include it in the TeXComm/TeXCommS's command field, so that
	-- the whitespace is not lost when serializing back to text when sending to MathJax.

replArgs :: [[Token]] -> [Token] -> [Token]
replArgs args = go
	where
		go [] = []
		go (Token "%" : (span (/= Token "\n") -> (x, y))) = Token "%" : x ++ go y
		go (Token "#" : Token "#" : y) = Token "#" : go y
		go (Token "#" : Token [getDigit -> Just i] : y)
			| length args >= i = (args !! (i-1)) ++ go y
			| otherwise = error $ "need more args than " ++ show args ++ " to replace in " ++ show (concatMap tokenChars y)
		go (x:y) = x : go y

parseBegin :: Context -> String -> [Token] -> ParseResult
parseBegin c env t
    | env `elem` ["codeblock", "itemdecl", "codeblockdigitsep"]
	, Just (code, rest) <- stripInfix [Token "\\end", Token "{", Token env, Token "}"] t
	= prependContent [TeXEnv env [] (parseCode c code)] (parse c rest)
parseBegin c env t
	| env == "codeblocktu"
	, Just (title, t') <- balanced ('{', '}') t
	, Just (code, rest) <- stripInfix [Token "\\end", Token "{", Token env, Token "}"] t'
	= prependContent [TeXEnv env [(FixArg, fullParse c title)] (parseCode c code)] (parse c rest)
parseBegin c "outputblock" t
    | Just (content, rest) <- stripInfix [Token "\\end", Token "{", Token "outputblock", Token "}"] t
	= prependContent [TeXEnv "outputblock" [] [TeXRaw $ Text.pack $ concatMap tokenChars content]] (parse c rest)
parseBegin c@Context{..} envname rest
	| Just Environment{..} <- Map.lookup (Text.pack envname) (environments macros)
	, not (envname `elem` dontEval) =
			let
				(args, bodyAndOnwards) = parseArgs envSignature rest
				(body, after_end) = balanced_body envname bodyAndOnwards
				together = replArgs args begin ++ body ++ end
				f
					| Just _ <- lookup envname makeEnv = (:[]) . TeXEnv envname (map ((FixArg, ) . fullParse c) args)
					| otherwise = id
				content = f $ fullParse c together
			in
				prependContent content (parse c after_end)
	| Just sig <- lookup envname makeEnv =
			let
				(arguments, rest') = parseArgs sig rest
				ParseResult body _ afterend = parse c rest'
				env = TeXEnv envname (map ((FixArg, ) . fullParse c) arguments) (concatRaws body)
					-- todo: not all fixargs
			in
				prependContent [env] (parse c afterend)
	| otherwise =
			let ParseResult body _ afterend = parse c rest
			in prependContent [TeXEnv envname [] (concatRaws body)] (parse c afterend)

parseDimen :: [Token] -> ([Token], [Token])
parseDimen toks
    | t@(Token txt) : more <- toks, txt `elem` [".", "pt"] || all isDigit txt = first (t :) (parseDimen more)
    | otherwise = ([], toks)

parseCmd :: Context -> String -> String -> [Token] -> ParseResult
parseCmd c@Context{..} cmd ws rest
	| cmd == "begin", Just (arg, rest') <- parseFixArg c rest =
		let [TeXRaw envname] = concatRaws arg in parseBegin c (Text.unpack envname) rest'
	| cmd == "end"
		, Just (_, rest') <- parseFixArg c rest = ParseResult mempty mempty rest'
	| cmd == "raisebox"
	, Just (a0, rest') <- balanced ('{', '}') rest
	, (a1, rest'') <- case parseOptArg rest' of
		Nothing -> (Nothing, rest')
		Just (x, y) -> (Just x, y)
	, Just (a2, rest''') <- balanced ('{', '}') rest'' =
		let
			args = [(FixArg, fullParse c a0)]
				++ case a1 of
					Nothing -> []
					Just x -> [(OptArg, fullParse c x)]
				++ [(FixArg, fullParse c a2)]
		in
			prependContent [TeXComm "raisebox" args] (parse c rest''')

	| cmd == "def"
	, (Token ('\\' : name) : rest') <- rest
	, Just (body, rest'') <- balanced ('{', '}') rest' =
		let
			m = Macros (Map.singleton name (Command (Signature 0 Nothing) body)) mempty mempty
			ParseResult p mm r = parse c{macros=macros++m} rest''
		in
			ParseResult p (m ++ mm) r
	| cmd == "def" = parse c $ snd $ fromJust $ balanced ('{', '}') $ dropWhile (/= Token "{") rest

	| cmd == "kern" = parse c $ snd $ parseDimen rest

	| Just signature <- lookup cmd signatures =
		let
			(args, rest') = parseArgs2 c signature rest
			content = TeXComm (cmd ++ ws) args
		in
			(if cmd `elem` kill then id else prependContent [content])
			(parse c rest')
	| otherwise = case Map.lookup cmd (commands macros) of
		Nothing -> error $
			"\n\nundefined command: " ++ show cmd ++
			" at: " ++ take 50 (concatMap tokenChars rest)
		Just Command{..} ->
			let
				(args, rest') = parseArgs cmdSignature rest
			in
				(if cmd `elem` kill then id
				 else prependContent $ fullParse c $ replArgs args cmdBody)
				(parse c rest')

parse :: Context -> [Token] -> ParseResult
parse c (d@(Token "$") : (span (/= d) -> (math, Token "$" : rest))) =
	prependContent [TeXMath Dollar (fullParse c math)] (parse c rest)
parse c (Token "\\[" : (span (/= Token "\\]") -> (math, Token "\\]" : rest))) =
	prependContent [TeXMath Square (fullParse c math)] (parse c rest)
parse c (Token "]" : x)
	| parsingOptArg c = ParseResult mempty mempty x
parse _ (Token "}" : x) = ParseResult mempty mempty x
parse c (Token "{" : x) =
	let ParseResult y _ rest = parse c x
	in prependContent [TeXBraces y] $ parse c rest
parse c (Token "%" : x)
	| commentsEnabled c = parse c (rmLine x)
parse _ [] = ParseResult mempty mempty mempty
parse c (Token "\\\\" : x) = prependContent [TeXLineBreak] (parse c x)
parse c (Token ['\\', ch] : x)
	| ch `elem` literal = prependContent [TeXComm [ch] []] (parse c x)
parse c (Token ('\\':'v':'e':'r':'b':':':arg) : rest) =
	prependContent [TeXComm "verb" [(FixArg, [TeXRaw $ Text.pack arg])]] (parse c rest)
parse c (Token "\\let" : _ : _ : s) = parse c s -- todo
parse c (Token "\\newcommand" : Token "{" : s) = parseNewCmd c s
parse c (Token "\\right" : Token "." : more) = prependContent [TeXComm "right" [(FixArg, [TeXRaw "."])]] (parse c more)
parse c (Token "\\renewcommand" : Token "{" : s) = parseNewCmd c s
parse c (Token "\\newenvironment" : Token "{" : s) = parseNewEnv c s
parse c (Token "\\lstnewenvironment" : Token "{" : s) = parseNewEnv c s
parse c (Token "\\rSec" : Token [getDigit -> Just i] : s)
		= prependContent [TeXComm "rSec" args] $ parse c s''
	where
		Just (a, s') = parseOptArg s
		Just (b, s'') = parseFixArg c s'
		args = [(FixArg, [TeXRaw $ Text.pack $ show i]), (FixArg, fullParse c a), (FixArg, b)]
parse c (Token ('\\' : cmd : ws) : rest)
	| all isSpace ws = parseCmd c [cmd] ws rest
parse c (Token ('\\' : (span (not . isSpace) -> (cmd, ws))) : rest) = parseCmd c cmd ws rest
parse ctx (Token c : rest)
	| all isAlphaNum c
		= prependContent [TeXRaw $ Text.pack c] $ parse ctx rest
parse ctx (Token [c] : rest)
	| isAlphaNum c || isSpace c || (c `elem` (".^|,[]':@-+=()!/;*~\"“”_<>&$?#" :: String))
		= prependContent [TeXRaw $ Text.pack [c]] $ parse ctx rest
parse _ s = error $ "parse: unexpected: " ++ take 100 (concatMap tokenChars s)

fullParse :: Context -> [Token] -> LaTeX
fullParse c t
	| all isSpace (concatMap tokenChars remainder) = concatRaws content
	| otherwise = error $ "could not fully parse: "
		++ concatMap tokenChars t
		++ "\n\nremainder: "
		++ concatMap tokenChars remainder
	where ParseResult{..} = parse c t
