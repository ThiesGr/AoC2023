parseCard :: [Char] -> ([String], [String])
parseCard cardString = (takeWhile ("|"/=) arr, drop 1 $ dropWhile ("|"/=) arr)
    where  
        arr = words . drop 1 $ dropWhile (':'/=) cardString

solve :: (Num a1, Foldable t1, Foldable t2, Eq a1, Eq a2) => (t1 a2, t2 a2) -> a1
solve (numbers, draw) = foldr (\_ acc -> if acc == 0 then 1 else acc * 2) 0 winning
    where
        winning = foldr (\x acc -> if x `elem` draw then x:acc else acc) [] numbers

main :: IO ()
main = do 
    input <- readFile "input.txt"
    let cards = map parseCard $ lines input
    let solution = sum $ map solve cards
    print solution