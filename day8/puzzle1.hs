{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
import Data.Map qualified as Map

data Element = Element {
    left :: String,
    right :: String
} deriving (Show)

solve :: Map.Map String Element -> String -> Int
solve network instructions = go "AAA" 0 $ cycle instructions
    where 
        go current count (x:xs)
            | current == "ZZZ" = count
            | x == 'L' = go left (count+1) xs
            | x == 'R' = go right (count+1) xs
            where 
                left = (network Map.! current).left
                right = (network Map.! current).right

parseInput :: [String] -> (String, Map.Map String Element)
parseInput (instructions:_:xs) = (instructions, Map.fromList network)
    where network = map (\x -> ((take 3 x), Element {left = (take 3 . drop 7) x, right = (take 3 . drop 12) x })) xs
main = do
    input <- readFile "input.txt"
    let (instructions, network) = parseInput $ lines input
    print $ solve network instructions