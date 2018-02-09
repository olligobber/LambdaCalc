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

-- Default variable names
variables :: [String]
variables = ((:[]) <$> ['a'..'z']) ++ ((++"'") <$> variables)

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

-- Recursively unbinds variables bound to the lambda n levels below
-- First variable is the expression to replace with
unbindAt :: Expression -> Integer -> Expression -> Expression
unbindAt t n (Lambda x) = Lambda (unbindAt t (n+1) x)
unbindAt t m (Bound n)
    | m == n    = t
    | otherwise = Bound n
unbindAt t n (Apply x y) = Apply (unbindAt t n x) (unbindAt t n y)
unbindAt _ _ (Unbound s) = Unbound s

-- Renders an expression, given an infinite list of possibly unused variables
-- TODO Remove unneccesary brackets
showExpression' :: Expression -> [String] -> String
showExpression' (Lambda x) (v:vs) =
    "λ" ++ v ++ "." ++ showExpression' (unbindAt (Unbound v) 1 x) vs
showExpression' (Bound _) _ = undefined -- Malformed expression
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
simplify (Lambda x) = Lambda <$> (simplify x)
simplify (Bound _) = Nothing
simplify (Unbound _) = Nothing
simplify (Apply (Lambda x) y) = case (simplify x, simplify y) of
    (Just a, Just b) -> Just (unbindAt b 1 a)
    (Just a, Nothing) -> Just (unbindAt y 1 a)
    (Nothing, Just b) -> Just (unbindAt b 1 x)
    (Nothing, Nothing) -> Just (unbindAt y 1 x)
simplify (Apply x y) = case (simplify x, simplify y) of
    (Just a, Just b) -> Just (Apply a b)
    (Just a, Nothing) -> Just (Apply a y)
    (Nothing, Just b) -> Just (Apply x b)
    (Nothing, Nothing) -> Nothing
