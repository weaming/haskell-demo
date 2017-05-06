import qualified Data.Char as C
import qualified Data.Map as M
import Data.List.Split
import Control.Monad

quicksort :: (Ord a) => [(k,a)] -> [(k,a)]
quicksort [] = []
quicksort ((k,a):xs) =
    let smallerSorted = quicksort [(x,y) | (x,y) <- xs, y<=a]
        biggerSorted = quicksort [(x,y) | (x,y) <- xs, y>a]
    in biggerSorted ++ [(k,a)] ++ smallerSorted

main = do
    txt <- getContents
    let wss = map (filter (\ w -> all C.isAlphaNum w && w /= "" )) $ map (splitOneOf " \t.+-*/|&^>=<\\_,.:;!?()[]{}'\"，。；？！") $ lines $ map C.toLower txt
    let countList = M.toList . M.fromListWith (+) $ map (\x -> (x, 1)) $ concat wss
    let countSorted = quicksort countList
    let maxLen = maximum $ map (\(x,_) -> length x) countList
    forM countSorted (\(a,b) -> do putStrLn $ a ++ take (maxLen - length a) (repeat ' ') ++ ": " ++ (show b))
