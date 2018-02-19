import ParseLambda
import ProcessLambda
import Data.Char (toLower)
import System.IO (hFlush, stdout)

-- Simplifies an expression up to a certain number of times
interpretTimes :: Maybe Integer -> (Integer,Expression) -> (Integer,Expression)
interpretTimes (Just 0) x = x
-- seq m forces the evaluation of m
interpretTimes n (m,x) = seq m $ case simplify x of
    Nothing -> (m, x)
    Just y -> interpretTimes (pred<$>n) (m+1,y)

-- Takes the result of interpretTimes and renders it
showInterpret :: (Integer, Expression) -> String
showInterpret (n, x) = first ++ ".\n" ++ second where
    first = case n of
        0 -> "Already fully simplified"
        1 -> "Simplified 1 time"
        k -> "Simplified " ++ show k ++ " times"
    second = showExpression x

help :: String
help = "Valid commands: parse, show, help, interpret, quit\n\
\   `parse x` will parse a lambda expression and store it\n\
\   `show` will show the stored expression\n\
\   `help` dispays this\n\
\   `interpet n [x]` will simplify the stored input n times, \n\
\       or until there is nothing more to simplify, \n\
\       where n is an integer or `forever`.\n\
\       Providing a lambda expression as the optional argument will\n\
\       parse it and interpret it.\n\
\    `quit` will quit"

isDecimal :: String -> Bool
isDecimal "" = True
isDecimal (x:xs) = x `elem` "1234567890" && isDecimal xs

-- Given a "number" input and an expression, produces output
interpret :: String -> Expression -> (Maybe Expression, Maybe String)
interpret n x
    | toLower (head n) == 'f'   = let
        (k, r) = interpretTimes Nothing (0,x)
        o = showInterpret (k,r)
        in (Just r, Just o)
    | isDecimal n               = let
        (k, r) = interpretTimes (Just (read n)) (0,x)
        o = showInterpret (k,r)
        in (Just r, Just o)
    | otherwise                 = (Nothing, Just "Invalid number")

-- Given the result of previous execution and a line of input, produces output
stateLoop :: Maybe Expression -> String -> (Maybe Expression, Maybe String)
stateLoop p s = case (toLower $ head $ head $ words s, tail $ words s) of
    ('p',t) -> case parse (unwords t) of
        Nothing -> (p, Just "Failed to parse")
        Just q -> (Just q, Just "Parsed")
    ('s',_) -> case p of
        Nothing -> (Nothing, Just "Nothing has been parsed")
        Just q -> (Just q, Just (showExpression q))
    ('h',_) -> (p, Just help)
    ('i',n:t) -> case (parse $ unwords t, p) of
        (Nothing, Nothing) -> (Nothing, Just "Nothing has been parsed")
        (Nothing, Just q) -> case interpret n q of
            (Nothing, o) -> (p, o)
            a -> a
        (Just q, _) -> case interpret n q of
            (Nothing, o) -> (p, o)
            a -> a
    ('i',_) -> (p, Just "Missing number")
    ('q',_) -> (Nothing, Nothing)
    _ -> (p, Just "Unrecognised command")

loop :: Maybe Expression -> IO ()
loop x = do
    putStr "> "
    hFlush stdout
    i <- getLine
    case stateLoop x i of
        (y, Just o) -> do
            putStrLn o
            loop y
        (_, Nothing) -> do
            putStrLn "Quitting"
            return ()

main :: IO ()
main = loop Nothing
