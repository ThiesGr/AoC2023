{-
    d = distance
    t = total time
    h = hold time

    d = (t-h) * h
    d = -h*h + t*h
    -h*h + t*h -d = 0

    We need to solve for d being the record and h being the solution. 

    This can be done using the quadratic formula
-}
solve :: (Integral a1, Integral a2) => (a2, a2) -> a1
solve (t, d) = 1 + (ceiling $ h2 - 1) - (floor $ h1 + 1) 
    where
        discriminant = t * t - 4 * d
        h1 = ( (fromIntegral t) - (sqrt $ fromIntegral discriminant )) / 2.0
        h2 = ( (fromIntegral t) + (sqrt $ fromIntegral discriminant )) / 2.0

parseInput :: [String] -> [(Int, Int)]
parseInput l = zip (map read $ drop 1 . words $ l !! 0) (map read $ drop 1 . words $ l !! 1)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let puzzle = parseInput $ lines input
    let solution = product $ map solve puzzle 
    print solution