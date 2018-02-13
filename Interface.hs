import ParseLambda
import ProcessLambda
import Data.Char (toLower)
import System.IO (hFlush, stdout)

type MI = Maybe Integer

-- Cap for integers, prevents memory issues
maxInt :: Integer
maxInt = 999999999

-- Simplifies an expression up to a certain number of times
interpretTimes :: MI -> (MI, Expression) -> (MI, Expression)
interpretTimes (Just 0) x = x
interpretTimes m (n,x) = case (n, simplify x) of
    (_, Nothing) -> (succ<$>n, x)
    (Nothing, Just y) -> interpretTimes (pred<$>m) (Nothing,y)
    (Just k, Just y) -> if k > maxInt
        then interpretTimes (pred<$>m) (Nothing,y)
        else interpretTimes (pred<$>m) (succ<$>n,y)

showInterpretation :: (MI, Expression) -> String
showInterpretation (n, x) = first ++ second where
    first = case n of
        Nothing -> "Simplified tons.\n"
        Just 0 -> "Already simplified.\n"
        Just 1 -> "Simplified 1 time.\n"
        Just k -> "Simplified " ++ show k ++ "times.\n"
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
    | toLower (head n) == 'f' =
        let (k, r) = interpretTimes Nothing (Just 0,x)
        in (Just r, Just (showInterpretation (k,r)))
    | isDecimal n =
        let (k, r) = interpretTimes (Just (read n)) (Just 0,x)
        in (Just r, Just (showInterpretation (k,r)))
    | otherwise = (Nothing, Just "Invalid number")

-- Given the result of previous execution and a line of input, produces output
stateLoop :: Maybe Expression -> String -> (Maybe Expression, Maybe String)
stateLoop p s = case words s of
    ('p':_):t -> case parse (unwords t) of
        Nothing -> (p, Just "Failed to parse")
        Just q -> (Just q, Just "Parsed")
    ('P':_):t -> case parse (unwords t) of
        Nothing -> (p, Just "Failed to parse")
        Just q -> (Just q, Just "Parsed")
    ('s':_):_ -> case p of
        Nothing -> (Nothing, Just "Nothing has been parsed")
        Just q -> (Just q, Just (showExpression q))
    ('S':_):_ -> case p of
        Nothing -> (Nothing, Just "Nothing has been parsed")
        Just q -> (Just q, Just (showExpression q))
    ('h':_):_ -> (p, Just help)
    ('H':_):_ -> (p, Just help)
    ('i':_):n:t -> case parse (unwords t) of
        Nothing -> case p of
            Nothing -> (Nothing, Just "Nothing has been parsed")
            Just q -> case interpret n q of
                (Nothing, o) -> (p, o)
                a -> a
        Just q -> case interpret n q of
            (Nothing, o) -> (p, o)
            a -> a
    ('I':_):n:t -> case parse (unwords t) of
        Nothing -> case p of
            Nothing -> (Nothing, Just "Nothing has been parsed")
            Just q -> case interpret n q of
                (Nothing, o) -> (p, o)
                a -> a
        Just q -> case interpret n q of
            (Nothing, o) -> (p, o)
            a -> a
    ('i':_):_ -> (p, Just "Missing number")
    ('I':_):_ -> (p, Just "Missing number")
    ('q':_):_ -> (Nothing, Nothing)
    ('Q':_):_ -> (Nothing, Nothing)
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
