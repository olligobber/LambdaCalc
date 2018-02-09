import ParseLambda (parse)
import ProcessLambda (showExpression, simplify, Expression)
import System.Environment (getArgs)

-- default maximum simplifies
maxSimplifies :: Int
maxSimplifies = 10000000

-- Simplifies an expression up to a certain number of times
interpretTimes :: Int -> Expression -> Expression
interpretTimes 0 x = x
interpretTimes n x = case simplify x of
    Nothing -> x
    Just y -> interpretTimes (n-1) y

-- Takes optional maximum simplifies as argument, and simplifies standard input
main :: IO ()
main = do
    args <- getArgs
    inp <- getContents
    case parse inp of
        Nothing -> ioError (userError "Failed to parse input")
        Just p -> case args of
            [] -> putStrLn $ showExpression $ interpretTimes maxSimplifies p
            n:_ -> putStrLn $ showExpression $ interpretTimes (read n) p
