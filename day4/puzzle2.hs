parseCard :: [Char] -> ([String], [String])
parseCard cardString = (takeWhile ("|"/=) arr, drop 1 $ dropWhile ("|"/=) arr)
    where  
        arr = words . drop 1 $ dropWhile (':'/=) cardString

wincount :: (Foldable t1, Foldable t2, Eq a) => (t1 a, t2 a) -> Int
wincount (numbers, draw) = length winning
    where
        winning = foldr (\x acc -> if x `elem` draw then x:acc else acc) [] numbers

solve :: [(Int, Int)] -> Int
solve cards = length $ go cards []
    where
        go todo done
            | length todo == 0 = done
            | otherwise = go newtodo (done ++ todo)
            where 
                newtodo = foldr (\card acc -> (copycards card) ++ acc ) [] todo
                copycards (number, wincount) = take wincount $ drop number cards

main :: IO ()
main = do 
    input <- readFile "input.txt"
    let cards = map parseCard $ lines input
    let wincounts = zip [1..] $ (map wincount cards)
    print $ solve wincounts
