import Data.List ( (!?) ) 

coords :: Foldable t => [t a] -> [(Int, Int)]
coords sketch = [(x,y) | x <- [0..(length $ sketch !! 0)-1], y <- [0 .. length sketch - 1]]

pipeAt :: (Int,Int) -> [String] -> Char
pipeAt (x,y) sketch = case sketch !? y of 
    Nothing -> '.'
    Just row -> case row !? x of
        Nothing -> '.'
        Just c -> c

findStart :: [String] -> (Int, Int)
findStart sketch = case (filter (\c -> 'S' == pipeAt c sketch) $ coords sketch) of
    [c] -> c
    [] -> error "No start"
    _ -> error "Multiple starts"


-- | is a vertical pipe connecting north and south.
-- - is a horizontal pipe connecting east and west.
-- L is a 90-degree bend connecting north and east.
-- J is a 90-degree bend connecting north and west.
-- 7 is a 90-degree bend connecting south and west.
-- F is a 90-degree bend connecting south and east.
-- . is ground; there is no pipe in this tile.
findLoop :: [String] -> [[(Int,Int)]]
findLoop sketch = map (\s -> go s [s, start] ) $ filter connectsToStart nodesAroundStart
    where
        isloop loop = case loop !? 0 of
            Nothing -> False
            Just p -> connectsToStart p
        go :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
        go pos@(x,y) route
            | pipe == '|' && (not $ (elem north route)) = go north $ north : route
            | pipe == '|' && (not $ (elem south route)) = go south $ south : route
            | pipe == '-' && (not $ (elem east  route)) = go east  $ east : route
            | pipe == '-' && (not $ (elem west  route)) = go west  $ west : route
            | pipe == 'L' && (not $ (elem north route)) = go north $ north : route
            | pipe == 'L' && (not $ (elem east  route)) = go east  $ east : route
            | pipe == 'J' && (not $ (elem north route)) = go north $ north : route
            | pipe == 'J' && (not $ (elem west  route)) = go west  $ west : route
            | pipe == '7' && (not $ (elem south route)) = go south $ south : route
            | pipe == '7' && (not $ (elem west  route)) = go west  $ west : route
            | pipe == 'F' && (not $ (elem south route)) = go south $ south : route
            | pipe == 'F' && (not $ (elem east  route)) = go east  $ east : route
            | otherwise = route
            where
                pipe = pipeAt pos sketch
                north = (x  , y-1)
                east  = (x+1, y)
                south = (x  , y+1)
                west  = (x-1, y)
        connectsToStart (x  , y) 
            | (x+1, y) == start && pipeAt (x,y) sketch `elem` ['-', 'L', 'F' ] = True
            | (x-1, y) == start && pipeAt (x,y) sketch `elem` ['-', 'J', '7' ] = True
            | (x  , y+1) == start && pipeAt (x,y) sketch `elem` ['|', 'F', '7'] = True
            | (x  , y-1) == start && pipeAt (x,y) sketch `elem` ['|', 'L', 'J'] = True
            | otherwise = False
        start = findStart sketch
        nodesAroundStart = [(fst start - 1, snd start), (fst start + 1, snd start), (fst start, snd start - 1), (fst start, snd start + 1)]

intersections :: (Int,Int) -> [String] -> [(Int,Int)] -> Int 
intersections pos@(x,y) sketch loop = go loopRow 0
    where
        go x count = case x of
            [] -> count
            ('|':xs) -> go xs (count+1)
            ('F':xs) -> case (dropWhile (not . flip elem ['J', '7']) xs) of
                ('J': xs') -> go xs' (count + 1)
                ('7': xs') -> go xs' (count + 2)
            ('L':xs) -> case (dropWhile (not . flip elem ['7', 'J']) xs) of
                ('J': xs') -> go xs' (count + 2)
                ('7': xs') -> go xs' (count + 1)
            (_:xs) -> go xs count 
        maxX = length (sketch !! y) - 1
        loopRow = map loopPipeAt $ zip [x..maxX] (repeat y)
        determineStart (x,y) 
            | loopPipeAt (x, y-1) `elem` ['|', '7', 'F'] && loopPipeAt (x, y+1) `elem` ['|', 'J', 'L'] = '|'
            | loopPipeAt (x-1, y) `elem` ['-', 'F', 'L'] && loopPipeAt (x, y-1) `elem` ['|', 'F', '7'] = 'J'
            | loopPipeAt (x-1, y) `elem` ['-', 'L', 'F'] && loopPipeAt (x+1, y) `elem` ['-', 'J', '7'] = '-'
            | loopPipeAt (x, y-1) `elem` ['|', '7', 'F'] && loopPipeAt (x+1, y) `elem` ['-', '7', 'J'] = 'L'
            | loopPipeAt (x-1, y) `elem` ['-', 'F', 'L'] && loopPipeAt (x, y+1) `elem` ['|', 'L', 'J'] = '7'
            | loopPipeAt (x+1, y) `elem` ['-', 'J', '7'] && loopPipeAt (x, y+1) `elem` ['|', 'J', 'L'] = 'F'

        loopPipeAt pos@(x,y)
            | not $ pos `elem` loop = '.'
            | sketch !! y !! x == 'S' = determineStart (x,y)
            | otherwise = sketch !! y !! x 

findEnclosed :: [(Int, Int)] -> [String] -> Int
findEnclosed loop sketch = foldr (\x acc -> if isEnclosed x then acc + 1 else acc) 0 $ coords sketch
    where
        isEnclosed coord@(x,y) 
            | coord `elem` loop = False
            | otherwise = (intersections (x+1, y) sketch loop) `mod` 2 == 1

main :: IO ()
main = do
    input <- readFile "input.txt"
    let sketch = lines input
    let loop = findLoop sketch !! 0
    print $ findEnclosed loop sketch