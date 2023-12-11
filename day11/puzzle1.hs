import Data.List (transpose)

expandRows :: [String] -> [String]
expandRows = foldr (\x acc-> if all (=='.') x then x:x:acc else x:acc) []

expandCols :: [String] -> [String]
expandCols = transpose . expandRows . transpose

expand :: [String] -> [String]
expand = expandCols . expandRows

galaxyPositions :: [String] -> [(Int,Int)]
galaxyPositions universe = filter (\(x,y) -> universe !! y !! x == '#') [(x,y) | x <- [0.. (length $ universe !! 0) - 1], y <- [0 .. (length universe) - 1]]

pathLengths :: [(Int, Int)] -> [Int]
pathLengths [] = []
pathLengths (x:xs) = foldr (\x' acc -> pathLength x x':acc) [] xs ++ pathLengths xs 
    where 
        pathLength (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2) 

main :: IO ()
main = do
    input <- readFile "input.txt"
    let universe = expand $ lines input
    let solution = sum . pathLengths $ galaxyPositions universe 
    print solution