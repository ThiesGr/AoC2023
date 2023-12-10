-- Compile with O2
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.List (isPrefixOf, uncons)

data Mapping =  Mapping {
    source :: Int,
    destination :: Int,
    length' :: Int
} deriving (Show)

data Puzzle = Puzzle {
    seeds :: [Int],
    seedToSoil :: [Mapping],
    soilToFertilizer :: [Mapping],
    fertilizerToWater :: [Mapping],
    waterToLight :: [Mapping],
    ligthToTemperature :: [Mapping],
    temperatureToHumidity :: [Mapping],
    humidityToLocation :: [Mapping]
} deriving (Show)

doMapping :: Mapping -> Int -> Maybe Int
doMapping m n
    | n < m.source = Nothing
    | n > (m.source + m.length' - 1) = Nothing
    | otherwise = Just $ m.destination + n - m.source 

doMappings :: [Mapping] -> Int -> Int
doMappings [] n = n
doMappings (m:ms) n = case doMapping m n of
    Just x -> x
    Nothing -> doMappings ms n

solveSeed :: Int ->  Puzzle -> Int
solveSeed n puzzle =
    doMappings puzzle.humidityToLocation $
    doMappings puzzle.temperatureToHumidity $
    doMappings puzzle.ligthToTemperature $
    doMappings puzzle.waterToLight $
    doMappings puzzle.fertilizerToWater $
    doMappings puzzle.soilToFertilizer $
    doMappings puzzle.seedToSoil n

solveSeedRange :: (Int, Int) -> Puzzle -> Int
solveSeedRange (s,l) puzzle = go s l (solveSeed s puzzle)
    where
        go :: Int -> Int -> Int -> Int 
        go n left acc
            | left == 1 = min acc (solveSeed n puzzle)
            | otherwise = go (n+1) (left-1) $! (min acc (solveSeed n puzzle))

solvePuzzle :: Puzzle -> [Int]
solvePuzzle puzzle = map (\n -> solveSeedRange n puzzle) $ seedsToTuple puzzle.seeds

head' :: [a] -> a
head' xs = case uncons xs of
    Just (x, _) -> x
    Nothing -> error "Empty list"

seedsToTuple :: [Int] -> [(Int, Int)]
seedsToTuple [] = []
seedsToTuple (s:l:xs) = (s, l) : seedsToTuple xs

parseInput :: [String] -> Puzzle
parseInput input = Puzzle {
        seeds = map read . words . drop 7 $ head' input,
        seedToSoil = getMaps "seed-to-soil",
        soilToFertilizer = getMaps "soil-to-fertilizer",
        fertilizerToWater = getMaps "fertilizer-to-water",
        waterToLight = getMaps "water-to-light",
        ligthToTemperature = getMaps "light-to-temperature",
        temperatureToHumidity = getMaps "temperature-to-humidity",
        humidityToLocation = getMaps "humidity-to-location"
    } 
    where
        toMap [d, s, l] = Mapping { source = read s, destination = read d, length' = read l}
        getMaps header = map (toMap . words) $ getSection header
        getSection header =
            takeWhile (/="") . drop 1 $ dropWhile (not . isPrefixOf header) input


main :: IO ()
main = do
    input <- readFile "input.txt"
    let puzzle = parseInput $ lines input
    print $ minimum $ solvePuzzle puzzle

