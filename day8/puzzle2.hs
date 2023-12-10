{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Map.Strict qualified as Map
import Data.List (isSuffixOf)
data Element = Element {
    left :: String,
    right :: String
} deriving (Show)

solve' :: String -> Map.Map String Element -> String -> Int
solve' start network instructions = go start 0 $ cycle instructions
    where 
        go current count (x:xs)
            | "Z" `isSuffixOf` current = count
            | x == 'L' = go left (count+1) xs
            | x == 'R' = go right (count+1) xs
            where 
                left = (network Map.! current).left
                right = (network Map.! current).right

solve :: [String] -> Map.Map String Element -> String -> Int
solve start network instructions = foldl1 lcm $ map (\x -> solve' x network instructions) start

parseInput :: [String] -> ([String], String, Map.Map String Element)
parseInput (instructions:_:xs) = (filter (\x -> last x == 'A') $ map (take 3) xs, instructions, Map.fromList network)
    where network = map (\x -> ((take 3 x), Element {left = (take 3 . drop 7) x, right = (take 3 . drop 12) x })) xs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (start, instructions, network) = parseInput $ lines input
    print $ solve start network $ cycle instructions