import Data.List
import System.Environment
import qualified Data.Char as C

solveRPN ::  String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = if all (\ x -> C.isDigit x || x `elem` '.') numberString then read numberString:xs else xs

main = do (expression:_) <- getArgs
          putStrLn . show . solveRPN $ expression
