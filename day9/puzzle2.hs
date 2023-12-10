parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

diff :: Num a => [a] -> [a]
diff xs = go xs []
    where 
        go xs result = case xs of
            (a:b:[]) -> result ++ [b - a]
            (a:b:xs) -> go (b:xs) (result ++ [(b-a)])

solve' :: [Int] -> Int
solve' = foldr1 (-) . map (!!0) . (\(a,b) -> a ++ [b !! 0]) . break (all (==0)) . iterate diff

solve :: [[Int]] -> Int
solve = sum . map solve'

main :: IO ()
main = do
    input <- readFile "input.txt"
    let report = parseInput input
    let solution = solve report
    print solution