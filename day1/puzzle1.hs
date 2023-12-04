import System.IO
import Data.Char (isDigit, digitToInt)
import Data.List (stripPrefix, uncons)
import Data.Maybe (isJust)

solve :: [String] -> Int
solve = sum . map number
    where
        first xs = case uncons xs of
            Just (x,_) -> x
            Nothing -> 0
        digits = map digitToInt . filter isDigit
        number s = (10 * (first (digits s))) + (last $ digits s)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ solve $ lines contents