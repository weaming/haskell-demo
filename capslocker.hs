import Data.Char

main = do
    c <- getContents
    putStr $ map toUpper c
