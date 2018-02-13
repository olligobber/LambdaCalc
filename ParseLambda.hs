module ParseLambda
( parse
) where

import Data.Char -- isAlphaNum, isSpace
import Control.Monad -- >=>
import Data.Function -- fix
import ProcessLambda
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map

-- Lambda Characters
isLambda :: Char -> Bool
isLambda '\\' = True
isLambda 'λ' = True
isLambda _ = False

-- Variable Characters
isVariable :: Char -> Bool
isVariable '"' = True
isVariable '\'' = True
isVariable '_' = True
isVariable '`' = True
isVariable 'λ' = False
isVariable x = isAlphaNum x

-- Characters where adjacent whitespace is irrelevant
isBoundary :: Char -> Bool
isBoundary '(' = True
isBoundary ')' = True
isBoundary '.' = True
isBoundary x = isLambda x || isSpace x

-- Filters whitespace unneccessary to parsing
filterWhitespace :: String -> String
filterWhitespace [] = []
filterWhitespace [x]
    | isSpace x = []
    | otherwise = [x]
filterWhitespace (x:y:zs)
    | isBoundary x && isSpace y = filterWhitespace (x:zs)
    | isSpace x && isBoundary y = filterWhitespace (y:zs)
    | otherwise                 = x : filterWhitespace (y:zs)

data ParseTree =
    Bind String ParseTree |
    Applies [ParseTree] |
    Variable String
--        deriving Show

data Token =
    Letter (Char -> Bool) | -- An actual letter of input to consume
    Symbol Char | -- A symbol of the CFG
    Action Int ([ParseTree] -> ParseTree) -- Action token

{- For debugging
instance Show Token where
    show (Letter _) = "Letter"
    show (Symbol a) = "Symbol " ++ [a]
    show (Action n _) = "Action " ++ show n
-}

type State = (String, [Token], [ParseTree])

-- Tokens to add to the stack for each production rule
prodRules :: [[Token]]
prodRules = [
{-0-}   [Letter isLambda, Symbol 'V', Letter (=='.'), Symbol 'A',
            Action 4 (\[a, _, Variable v, _] -> Applies [Bind v a])],
{-1-}   [Symbol 'V', Symbol 'B', Action 2 (\[Applies b, v] -> Applies (v:b))],
{-2-}   [Letter (=='('), Symbol 'A', Letter (==')'), Symbol 'C',
            Action 4 (\[Applies c, _, a, _] -> Applies (a:c))],
{-3-}   [Letter isSpace, Symbol 'A', Action 2 (\[a,_] -> a)],
{-4-}   [Action 0 (\[] -> Applies [])],
{-5-}   [Letter isVariable, Symbol 'W',
            Action 2 (\[Variable w, Variable [v]] -> Variable (v:w))],
{-6-}   [Action 0 (\[] -> Variable "")]]

-- Given the next character of the input and a symbol, choose a production rule
-- See LL(1) Parse Table in lambda_cfg file for explanation
chooseProd :: Maybe Char -> Char -> Maybe Int
chooseProd Nothing t
    | t `elem` "BC"     = Just 4
    | t == 'W'          = Just 6
    | otherwise         = Nothing
chooseProd (Just s) 'W'
    | isVariable s      = Just 5
    | s `elem` "()."    = Just 6
    | isSpace s         = Just 6
    | isLambda s        = Just 6
    | otherwise         = Nothing
chooseProd (Just s) 'V'
    | isVariable s      = Just 5
    | otherwise         = Nothing
chooseProd (Just '(') t
    | t `elem` "ABC"    = Just 2
    | otherwise         = Nothing
chooseProd (Just ')') t
    | t `elem` "BC"     = Just 4
    | otherwise         = Nothing
chooseProd (Just s) 'A'
    | isVariable s      = Just 1
    | isLambda s        = Just 0
    | otherwise         = Nothing
chooseProd (Just s) 'B'
    | isSpace s         = Just 3
    | isLambda s        = Just 0
    | otherwise         = Nothing
chooseProd (Just s) 'C'
    | isVariable s      = Just 1
    | isLambda s        = Just 0
    | otherwise         = Nothing
chooseProd _ _ = Nothing

{-
Takes the current state of parsing and moves forward a step, yielding
    Left Nothing when input is invalid
    Left (Just p) when parsing finishes, p is the Parse Tree
    Right s when parsing is ongoing and s is the next state
-}
parseStep :: State -> Either (Maybe ParseTree) State
parseStep ("",[],[p]) = Left (Just p)
parseStep (a:as, Letter f:ts, ps)
    | f a           = Right (as, ts, Variable [a]:ps)
    | otherwise     = Left Nothing
parseStep (ss, (Action n f):ts, ps) =
    Right (ss, ts, f (take n ps):drop n ps)
parseStep ("", Symbol t:ts, ps) = case chooseProd Nothing t of
    Just n -> Right ("", (prodRules!!n)++ts, ps)
    Nothing -> Left Nothing
parseStep (ss@(s:_), Symbol t:ts, ps) = case chooseProd (Just s) t of
    Just n -> Right (ss, (prodRules!!n)++ts, ps)
    Nothing -> Left Nothing
parseStep _ = Left Nothing

{-
To apply parseStep n times to a state s, do
(foldl1 (>=>) $ replicate n parseStep) s
-}

-- Applies parseStep repeatedly until a Left is returned
parseTree' :: String -> Maybe ParseTree
parseTree' s = p where
    Left p = fix (parseStep >=>) (s,[Symbol 'A'],[])

-- Filters whitespace, then parses
parseTree :: String -> Maybe ParseTree
parseTree = parseTree' . filterWhitespace . dropWhile isSpace

{-
Converts a parse tree to a lambda expression, given a map from bound variables
to the level they were bound at, and the current level
-}
convert' :: Map String Int -> Int -> ParseTree -> Expression
convert' m n (Bind a x) = Lambda (convert' m2 (n+1) x) where
    m2 = Map.insert a n m
convert' m n (Applies [a]) = convert' m n a
convert' m n (Applies [a,b]) = Apply (convert' m n a) (convert' m n b)
convert' m n (Applies (a:b:cs)) = convert' m n (Applies (Applies [a,b]:cs))
convert' m n (Variable x)
    | Map.member x m    = Bound (toInteger(n - (m!x)))
    | otherwise         = Unbound x
convert' _ _ _ = undefined

convert :: ParseTree -> Expression
convert = convert' Map.empty 0

parse :: String -> Maybe Expression
parse s = convert <$> (parseTree s)
