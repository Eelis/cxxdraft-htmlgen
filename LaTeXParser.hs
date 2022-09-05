{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TupleSections #-}

module LaTeXParser (parseString,
	Token(Token), Context(..), defaultContext, Signature(..), Macros(..), Environment(..), Command(..), ParseResult(ParseResult),
	defaultMacros,
	nullCmd, storeCmd, codeEnv, normalCmd,
	storeEnv) where

import LaTeXBase (LaTeXUnit(..), LaTeX, TeXArg, ArgKind(..), MathType(..), concatRaws)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isAlphaNum, isSpace, isAlpha)
import Data.Maybe (fromJust)
import Control.Arrow (first)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding ((++), (.))
import Util ((.), (++), getDigit, stripInfix)

newtype Token = Token { tokenChars :: String }
	deriving (Eq, Show)

data Environment = Environment (Context -> [Token] -> ParseResult)
data Command = Command { runCommand :: Context -> String {- ws -} -> [Token] -> ParseResult }

data Macros = Macros
	{ commands :: Map Text Command
	, environments :: Map Text Environment
	, counters :: Map Text Int }

newCommand :: Bool {- overwrite -} -> (Text, Command) -> Macros -> Macros
newCommand True (name, cmd) Macros{..} = Macros{commands = Map.insert name cmd commands, ..}
newCommand False (name, cmd) Macros{..} = Macros{commands = Map.insertWith (\_ y -> y) name cmd commands, ..}

instance Semigroup Macros where
	x <> y = Macros
		(commands x ++ commands y)
		(environments x ++ environments y)
		(counters x ++ counters y)

instance Monoid Macros where
	mempty = Macros mempty mempty mempty

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
	, macros :: Macros }

prependContent :: LaTeX -> ParseResult -> ParseResult
prependContent t p = p{content = t ++ content p}

combineMacros :: Bool {- left biased -} -> Macros -> Macros -> Macros
combineMacros b x y = if b then x ++ y else y ++ x

addMacros :: Bool {- overwrite -} -> Macros -> ParseResult -> ParseResult
addMacros b m p = p{newMacros = combineMacros b m (newMacros p)}

defaultEnvs :: [(Text, Environment)]
defaultEnvs = [outputblockEnv]

codeEnv :: Text -> Signature -> (Text, Environment)
codeEnv name sig = (name, Environment f)
	where
		f :: Context -> [Token] -> ParseResult
		f ctx toks = ParseResult [env] mempty rest'
			where
				(arguments, rest) = parseArgs sig toks
				Just (code, rest') = stripInfix [Token "\\end", Token "{", Token (Text.unpack name), Token "}"] rest
				env = TeXEnv (Text.unpack name) (map ((FixArg, ) . fullParse ctx) arguments) (parseCode name ctx code)

outputblockEnv :: (Text, Environment)
outputblockEnv = ("outputblock", Environment f)
	where
		f :: Context -> [Token] -> ParseResult
		f ctx toks = ParseResult [env] mempty rest
			where
				Just (content, rest) = stripInfix [Token "\\end", Token "{", Token "outputblock", Token "}"] toks
				env = TeXEnv "outputblock" [] (parseOutputBlock ctx content)

parseOutputBlock :: Context -> [Token] -> LaTeX
parseOutputBlock c = concatRaws . go
	where
		go :: [Token] -> LaTeX
		go [] = []
		go (Token "@" : rest) = fullParse c cmd ++ go rest'
			where (cmd, Token "@" : rest') = break (== Token "@") rest
		go s = TeXRaw (Text.pack $ concatMap tokenChars code) : go rest
			where (code, rest) = break (== Token "@") s

storeEnv :: String -> Signature -> (Text, Environment)
storeEnv name sig = (Text.pack name, Environment act)
	where
		act :: Context -> [Token] -> ParseResult
		act ctx toks = ParseResult [env] mempty afterend
			where
				(arguments, rest) = parseArgs sig toks
				ParseResult body _ afterend = parse ctx rest
				env = TeXEnv name (map ((FixArg, ) . fullParse ctx) arguments) (concatRaws body)
							-- todo: not all fixargs

defaultCmds :: [(Text, Command)]
defaultCmds =
	[ ("newcommand", newCommandCommand)
	, ("renewcommand", newCommandCommand)
	, ("newcolumntype", newColumnTypeCommand)
	, ("newenvironment", newEnvCommand)
	, ("lstnewenvironment", newEnvCommand)
	, ("raisebox", raiseBoxCommand)
	, ("let", Command $ \ctx _ws rest -> parse ctx (drop 2 rest))
	, beginCommand
	, endCommand
	, oldDefCommand
	]

oldDefCommand :: (Text, Command)
oldDefCommand = ("def", Command pars)
	where
		pars ctx@Context{..} _ws rest
			| (Token ('\\' : name) : rest') <- rest
			, Just (body, rest'') <- balanced ('{', '}') rest' =
				let
					m = Macros (Map.fromList [defCmd (Text.pack name) (Signature 0 Nothing) body]) mempty mempty
					ParseResult p mm r = parse ctx{macros=macros++m} rest''
				in
					ParseResult p (m ++ mm) r
			| otherwise = parse ctx $ snd $ fromJust $ balanced ('{', '}') $ dropWhile (/= Token "{") rest

endCommand :: (Text, Command)
endCommand = ("end", Command $ \c _ws rest ->
	let Just (_, rest') = parseFixArg c rest in ParseResult mempty mempty rest')

beginCommand :: (Text, Command)
beginCommand = ("begin", normalCmd $ Command pars)
	where
		pars c@Context{..} _ws rest
			| Just (Environment f) <- Map.lookup (envname) (environments macros) = f c rest'
			| otherwise = error $ "undefined env: " ++ Text.unpack envname
			where
				Just (arg, rest') = parseFixArg c rest
				[TeXRaw envname] = concatRaws arg

raiseBoxCommand :: Command
raiseBoxCommand = normalCmd $ Command $ \c@Context{..} _ws rest ->
	let
		Just (a0, rest') = balanced ('{', '}') rest
		(a1, rest'') = case parseOptArg rest' of
			Nothing -> (Nothing, rest')
			Just (x, y) -> (Just x, y)
		Just (a2, rest''') = balanced ('{', '}') rest''
		args = [(FixArg, fullParse c a0)]
			++ case a1 of
				Nothing -> []
				Just x -> [(OptArg, fullParse c x)]
			++ [(FixArg, fullParse c a2)]
	in
		ParseResult [TeXComm "raisebox" "" args] mempty rest'''

newCommandCommand :: Command
newCommandCommand = normalCmd $ Command $ \Context{..} _ws (Token "{" : Token ('\\' : name) : Token "}" : rest) ->
	let
		(sig, rest') = parseSignature rest
		Just (body, rest'') = balanced ('{', '}') rest'
		newMacros = newCommand True (defCmd (Text.pack name) sig body) mempty
	in
		ParseResult [] newMacros rest''

newColumnTypeCommand :: Command
newColumnTypeCommand = normalCmd $ Command $ \Context{..} _ws (Token "{" : Token _ : Token "}" : rest) ->
	let
		(_, rest') = parseSignature rest
		Just (_, rest'') = balanced ('{', '}') rest'
	in
		ParseResult [] mempty rest''

defaultMacros :: Macros
defaultMacros = Macros (Map.fromList defaultCmds) (Map.fromList defaultEnvs) mempty

defaultContext :: Context
defaultContext = Context
	{ commentsEnabled = True
	, parsingOptArg = False
	, macros = defaultMacros }

rmLine :: [Token] -> [Token]
rmLine s = case dropWhile (/= Token "\n") s of
	Token "\n" : x -> x
	x -> x

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

balanced_body :: Context -> String -> [Token] -> ([Token], [Token])
balanced_body ctx end = go 0
	where
		go :: Int -> [Token] -> ([Token], [Token])
		go 0 [] = ([], [])
		go 0 (Token "\\end" : Token "{" : e : Token "}" : x) | fullParse ctx [e] == [TeXRaw $ Text.pack end] = ([], x)
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

newEnvCommand :: Command
newEnvCommand = normalCmd $ Command $ \Context{..} _ws (Token "{" : (span (/= Token "}") -> (name, Token "}" : rest))) ->
	let
		nameStr = concatMap tokenChars name
		(sig, rest') = parseSignature rest
		Just (begin, rest'') = balanced ('{', '}') rest'
		Just (end, rest''') = balanced ('{', '}') rest''
		pa :: Context -> [Token] -> ParseResult
		pa c' toks = ParseResult replaced mempty toks''
			where
				replaced = fullParse c' $ replArgs args begin ++ body ++ end
				(args, toks') = parseArgs sig toks
				(body, toks'') = balanced_body c' nameStr toks'
		m = Macros mempty (Map.singleton (Text.pack nameStr) (Environment pa)) mempty
	in
		ParseResult [] m rest'''

parseString :: Context -> String -> (LaTeX, Macros, [Token])
parseString c s = (concatRaws x, y, z)
	where ParseResult x y z = parse c (tokenize s)

literal :: String
literal = " @_{}&,%-#/~>!$;:^"

breakComment :: [Token] -> ([Token], [Token])
breakComment x@(Token "\n" : _) = ([], x)
breakComment (Token ('\\' : cmd) : xs)
	| (c, r@(_:_)) <- span (/= '\n') cmd = ([Token ('\\':c)], Token r : xs)
breakComment (Token "%" : Token "\n" : x) = first ((Token "%" :) . (Token "\n" :)) (breakComment x)
breakComment (x : xs) = first (x:) (breakComment xs)
breakComment [] = ([], [])

data LiteralKind = StringLiteral | CharLiteral

parseCode :: Text -> Context -> [Token] -> LaTeX
parseCode envname c = concatRaws . go Nothing
	where
		go :: Maybe LiteralKind -> [Token] -> LaTeX
		go _ [] = []
		go b (Token "@" : rest) = fullParse c cmd ++ go b rest'
			where (cmd, Token "@" : rest') = break (== Token "@") rest
		go (Just StringLiteral) (Token "\"" : rest) = TeXRaw "\"" : go Nothing rest
		go (Just CharLiteral) (Token "'" : rest) = TeXRaw "'" : go Nothing rest
		go Nothing (Token "\"" : rest) = TeXRaw "\"" : (go (Just StringLiteral) lit ++ go Nothing rest')
			where (lit, rest') = stringLiteral rest
		go Nothing (Token "'" : rest)
		       | envname == "codeblockdigitsep" = TeXRaw "'" : go Nothing rest
		       | otherwise = TeXRaw "'" : (go (Just CharLiteral) lit ++ go Nothing rest')
		       where (lit, rest') = charLiteral rest
		go Nothing (Token "/" : Token "/" : (breakComment -> (comment, rest')))
			= TeXComm "comment" "" [(FixArg, TeXRaw "//" : noncode comment)] : go Nothing rest'
		go Nothing (Token "/" : Token "*" : rest)
		    | Just (comment, rest') <- stripInfix [Token "*", Token "/"] rest
		    = TeXComm "comment" "" [(FixArg, [TeXRaw "/*"] ++ noncode comment ++ [TeXRaw "*/"])] : go Nothing rest'
		go b (Token "/" : rest) = TeXRaw "/" : go b rest
		go b s = TeXRaw (Text.pack $ concatMap tokenChars code) : go b rest
			where
			  breakToks = [Token "@", Token "/"] ++ 
			    case b of
			      Nothing -> [Token "\"", Token "'"]
			      Just StringLiteral -> [Token "\""]
			      Just CharLiteral -> [Token "'"]
			  (code, rest) = break (`elem` breakToks) s
		noncode :: [Token] -> LaTeX
		noncode toks =
		  fullParse c nc ++ case more of
		    [] -> []
		    Token "@" : (break (== Token "@") -> (code, _ : rest)) ->
		        TeXComm "tcode" "" [(FixArg, fullParse c code)] : noncode rest
		    _ -> error "no"
		    where (nc, more) = span (/= Token "@") toks
		stringLiteral :: [Token] -> ([Token], [Token])
		stringLiteral (Token "\\" : Token "\"" : x) = first (Token "\\\"" :) (stringLiteral x)
		stringLiteral (Token "\\" : Token "\\" : x) = first (Token "\\\\" :) (stringLiteral x)
		stringLiteral (Token "\"" : x) = ([Token "\""], x)
		stringLiteral (y : x) = first (y :) (stringLiteral x)
		stringLiteral [] = ([], [])
		charLiteral :: [Token] -> ([Token], [Token])
		charLiteral (Token "\\" : Token "'" : x) = first (Token "\\'" :) (charLiteral x)
		charLiteral (Token "\\" : Token "\\" : x) = first (Token "\\\\" :) (charLiteral x)
		charLiteral (Token "'" : x) = ([Token "'"], x)
		charLiteral (y : x) = first (y :) (charLiteral x)
		charLiteral [] = ([], [])

tokenize :: String -> [Token]
tokenize "" = []
tokenize ('\\':'v':'e':'r':'b': delim : (break (== delim) -> (arg, _ : rest))) =
	Token ("\\verb:" ++ arg) : tokenize rest
tokenize ('\\' : (span isAlpha -> (cmd@(_:_), (span isSpace -> (ws, rest)))))
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

nullCmd :: Text -> Signature -> (Text, Command)
nullCmd name sig = defCmd name sig []

storeCmd :: String -> Signature -> (Text, Command)
storeCmd name sig = (Text.pack name, normalCmd $ Command pars)
	where
		pars context ws tokens = ParseResult [TeXComm name ws args] mempty rest
			where (args, rest) = parseArgs2 context sig tokens

defCmd :: Text -> Signature -> [Token] -> (Text, Command)
defCmd name sig body = (name, normalCmd $ Command pars)
	where
		pars context _ws tokens = ParseResult (fullParse context $ replArgs args body) mempty rest
			where (args, rest) = parseArgs sig tokens

normalCmd :: Command -> Command
normalCmd (Command f) = Command $ \ctx ws toks ->
	let ParseResult content newMacros rest = f ctx ws toks
	in addMacros False newMacros (prependContent content (parse ctx{macros=macros ctx ++ newMacros} rest))

parse :: Context -> [Token] -> ParseResult
parse c (d@(Token "$") : (span (/= d) -> (math, Token "$" : rest))) =
	prependContent [TeXMath Dollar (fullParse c math)] (parse c rest)
parse c (Token "\\[" : (span (/= Token "\\]") -> (math, Token "\\]" : rest))) =
	prependContent [TeXMath Square (fullParse c math)] (parse c rest)
parse c (Token "]" : x)
	| parsingOptArg c = ParseResult mempty mempty x
parse _ (Token "}" : x) = ParseResult mempty mempty x
parse c (Token "{" : x) = prependContent [TeXBraces y] $ parse c rest
	where ParseResult y _ rest = parse c x
parse c (Token "%" : x)
	| commentsEnabled c = parse c (rmLine x)
parse _ [] = ParseResult mempty mempty mempty
parse c (Token "\\\\" : x) = prependContent [TeXLineBreak] (parse c x)
parse c (Token ['\\', ch] : x)
	| ch `elem` literal = prependContent [TeXComm [ch] "" []] (parse c x)
parse c (Token ('\\':'v':'e':'r':'b':':':arg) : rest) =
	prependContent [TeXComm "verb" "" [(FixArg, [TeXRaw $ Text.pack arg])]] (parse c rest)
parse c (Token "\\rSec" : Token [getDigit -> Just i] : s)
		= prependContent [TeXComm "rSec" "" args] $ parse c s''
	where
		Just (a, s') = parseOptArg s
		Just (b, s'') = parseFixArg c s'
		args = [(FixArg, [TeXRaw $ Text.pack $ show i]), (FixArg, fullParse c a), (FixArg, b)]
parse c@Context{..} (Token ('\\' : (span (not . isSpace) -> (nos, w))) : rest)
	| Just f <- Map.lookup (Text.pack cmd) (commands macros) = runCommand f c ws rest
	| otherwise = error $
		"\n\nundefined command: " ++ show cmd ++ " at: " ++ take 50 (concatMap tokenChars rest)
	where (cmd, ws) | nos == "", (x : xx) <- w = ([x], xx)
	                | otherwise = (nos, w)
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
