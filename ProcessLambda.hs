module ProcessLambda
( Expression(..)
, showExpression
, simplify
) where

import Data.Set (Set)
import qualified Data.Set as Set

-- An expression of lambda calculus
data Expression =
    Lambda Expression |
    Bound Integer |
    Apply Expression Expression |
    Unbound String
--    deriving Show

-- Default variable names
variables :: [String]
variables = (pure <$> ['a'..'z']) ++ ((++"'") <$> variables)

{-
-- Determines if a variable is unbound
isUnbound :: String -> Expression -> Bool
isUnbound s (Lambda x) = isUnbound s x
isUnbound _ (Bound _) = True
isUnbound s (Apply x y) = isUnbound s x || isUnbound s y
isUnbound s (Unbound t) = s /= t
-}

-- Finds all unbound variables in an expression
unboundVariables :: Expression -> Set String
unboundVariables (Lambda x) = unboundVariables x
unboundVariables (Bound _) = Set.empty
unboundVariables (Apply x y) =
    Set.union (unboundVariables x) (unboundVariables y)
unboundVariables (Unbound s) = Set.singleton s

{-
increaseDepth k n x replaces all binds in x to a level more than n
with a bind k levels below that
-}
increaseDepth :: Integer -> Integer -> Expression -> Expression
increaseDepth k n (Lambda x) = Lambda (increaseDepth k (n+1) x)
increaseDepth k n (Bound m)
    | m > n     = Bound (m+k)
    | otherwise = Bound m
increaseDepth k n (Apply x y) =
    Apply (increaseDepth k n x) (increaseDepth k n y)
increaseDepth _ _ (Unbound s) = Unbound s

-- Recursively unbinds variables bound to the lambda n levels below
-- First variable is the expression to replace with
unbindAt :: Expression -> Integer -> Expression -> Expression
unbindAt t n (Lambda x) = Lambda (unbindAt t (n+1) x)
unbindAt t m (Bound n)
    | m == n    = increaseDepth (n-1) 0 t
    | otherwise = Bound n
unbindAt t n (Apply x y) = Apply (unbindAt t n x) (unbindAt t n y)
unbindAt _ _ (Unbound s) = Unbound s

-- Renders an expression, given an infinite list of possibly unused variables
-- TODO Remove unneccesary brackets
showExpression' :: Expression -> [String] -> String
showExpression' (Lambda x) (v:vs) =
    "λ" ++ v ++ "." ++ showExpression' (unbindAt (Unbound v) 1 x) vs
showExpression' (Lambda _) _ = error "Error: Run out of variable names"
showExpression' (Bound _) _ = error "Error: Malformed expression"
showExpression' (Apply (Lambda x) y) v =
    "(" ++ showExpression' (Lambda x) v ++ ") " ++ showExpression' y v
showExpression' (Apply x y) v =
    "(" ++ showExpression' x v ++ " " ++ showExpression' y v ++ ")"
showExpression' (Unbound s) _ = s

-- Renders an expression
showExpression :: Expression -> String
showExpression x = showExpression' x bound where
    bound = filter (`Set.notMember` unboundVariables x) variables

-- Simplifies an expression by applying lambdas where possible
-- Returns Nothing if no simplification could be done
simplify :: Expression -> Maybe Expression
simplify (Lambda x) = Lambda <$> simplify x
simplify (Bound _) = Nothing
simplify (Unbound _) = Nothing
simplify (Apply (Lambda x) y) = Just $ increaseDepth (-1) 1 $
    case (simplify x, simplify y) of
        (Just a, Just b) -> unbindAt b 1 a
        (Just a, Nothing) -> unbindAt y 1 a
        (Nothing, Just b) -> unbindAt b 1 x
        (Nothing, Nothing) -> unbindAt y 1 x
simplify (Apply x y) = case (simplify x, simplify y) of
    (Just a, Just b) -> Just (Apply a b)
    (Just a, Nothing) -> Just (Apply a y)
    (Nothing, Just b) -> Just (Apply x b)
    (Nothing, Nothing) -> Nothing
