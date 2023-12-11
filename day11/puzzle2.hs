import Data.List (transpose)

emptyRows :: [String] -> [Int]
emptyRows rows = filter (\x -> all (=='.') (rows !! x)) [0..length rows - 1]

emptyCols :: [String] -> [Int]
emptyCols = emptyRows . transpose

galaxyPositions :: [String] -> [(Int,Int)]
galaxyPositions universe = map correctPosition $ filter (\(x,y) -> universe !! y !! x == '#') [(x,y) | x <- [0.. (length $ universe !! 0) - 1], y <- [0 .. (length universe) - 1]]
    where
        correctedX = scanl (\acc x' -> if x' `elem` emptyCols universe then acc+1000000 else acc+1) 0 [0 .. length (universe !! 0) - 1]
        correctedY = scanl (\acc x' -> if x' `elem` emptyRows universe then acc+1000000 else acc+1) 0 [0 .. length universe - 1]        
        correctPosition (x,y) = (correctedX !! x,correctedY !! y)

pathLengths :: [(Int, Int)] -> [Int]
pathLengths [] = []
pathLengths (x:xs) = foldr (\x' acc -> pathLength x x':acc) [] xs ++ pathLengths xs 
    where 
        pathLength (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2) 

main :: IO ()
main = do
    input <- readFile "input.txt"
    let universe = lines input 
    let solution = sum . pathLengths $ galaxyPositions universe 
    print solution
