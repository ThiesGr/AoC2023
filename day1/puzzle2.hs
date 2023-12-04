import System.IO
import Data.Char (isDigit, digitToInt)
import Data.List (uncons, unsnoc, isPrefixOf)

digits :: [Char] -> [Int]
digits xs = go xs
    where 
        first xs = case (uncons xs) of
            Just (x, _) -> x
            Nothing -> error "Invalid input"
        go xs
            | xs == [] = []
            | "one"     `isPrefixOf` xs = 1 : go (drop 1 xs) 
            | "two"     `isPrefixOf` xs = 2 : go (drop 1 xs)
            | "three"   `isPrefixOf` xs = 3 : go (drop 1 xs)
            | "four"    `isPrefixOf` xs = 4 : go (drop 1 xs)
            | "five"    `isPrefixOf` xs = 5 : go (drop 1 xs)
            | "six"     `isPrefixOf` xs = 6 : go (drop 1 xs)
            | "seven"   `isPrefixOf` xs = 7 : go (drop 1 xs)
            | "eight"   `isPrefixOf` xs = 8 : go (drop 1 xs)
            | "nine"    `isPrefixOf` xs = 9 : go (drop 1 xs)
            | isDigit $ (first xs) = digitToInt (first xs) : digits (drop 1 xs)
            | otherwise = digits (drop 1 xs)

solve :: [String] -> Int
solve = sum . map number
    where
        first :: String -> Int
        first xs = case (uncons $ digits xs) of
            Just (x, _) -> x
            Nothing -> error "Invalid input"
        last :: String -> Int
        last xs = case (unsnoc $ digits xs) of
            Just (_,x) -> x
            Nothing -> error "Invalid input"
        number s = (10 * first s) + (last s)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print (solve $ lines contents)