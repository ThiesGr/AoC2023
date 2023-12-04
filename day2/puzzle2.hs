{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Text qualified as T
import Data.Char (isDigit)
import Data.List (find)

data Set = Set {
    red :: Int,
    green :: Int,
    blue:: Int
} deriving (Show)

data Game = Game {
    id :: Int,
    sets :: [Set]
} deriving (Show)

splitSets :: String -> [[(String, String)]]
splitSets = map (
    map ((\(n,c) -> (T.unpack $ T.strip c,T.unpack n)) . T.breakOn (T.pack " ") . T.strip) .
    T.splitOn (T.pack ",")) . T.splitOn (T.pack ";") . T.drop 1. T.dropWhile (/= ':') . T.pack

parseSet :: [(String, String)] -> Set
parseSet xs = Set { green=green, blue=blue, red=red}
    where
        green = case find (\(c,_) -> c == "green") xs of
            Just (_, n) -> read n
            Nothing -> 0
        blue = case find (\(c,_) -> c == "blue") xs of
            Just (_, n) -> read n
            Nothing -> 0
        red = case find (\(c,_) -> c == "red") xs of
            Just (_, n) -> read n
            Nothing -> 0

readGame :: String -> Game
readGame s = Game {
    id = id s, sets = sets s
}
    where
        id = read . takeWhile isDigit . drop 5
        sets = map parseSet . splitSets

minPower :: Game -> Int
minPower g = red * green * blue
    where
        green = foldr (max) 0 $ map (\s -> s.green) g.sets
        red = foldr (max) 0 $ map (\s -> s.red) g.sets
        blue = foldr (max) 0 $ map (\s -> s.blue) g.sets

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let games = map readGame $ lines contents
    print(sum $ map minPower games)