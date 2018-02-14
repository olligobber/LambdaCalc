import ParseLambda (parse)
import ProcessLambda (showExpression, simplify, Expression)
import System.Environment (getArgs)

-- default maximum simplifies
maxSimplifies :: Integer
maxSimplifies = 1000000

-- Simplifies an expression up to a certain number of times
interpretTimes :: (Integer, Expression) -> Expression
interpretTimes (0, x) = x
interpretTimes (n, x) = case simplify x of
    Nothing -> x
    Just y -> interpretTimes (n-1, y)

-- Takes optional maximum simplifies as argument, and simplifies standard input
main :: IO ()
main = do
    args <- getArgs
    inp <- getContents
    let put = putStrLn . showExpression . interpretTimes
        in case (parse inp, args) of
            (Nothing, _) -> ioError (userError "Failed to parse input")
            (Just p, []) -> put (maxSimplifies, p)
            (Just p, n:_) -> put (read n, p)
