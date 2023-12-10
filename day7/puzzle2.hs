import Data.List 
import Data.Char ( isDigit )


parseInput :: [String] -> [Hand]
parseInput = map (\(card:score:_)-> Hand (Card card) (read score)) . map words

data Hand = Hand Card Int
    deriving (Show)

data Card = Card String 
    deriving (Show)

instance Eq Hand where
    (==) (Hand (Card a) _) (Hand (Card b) _) = a == b  

instance Ord Hand where
    compare (Hand (Card a) _) (Hand (Card b) _)
            | a == b = EQ
            | (type' a) > (type' b) = GT
            | (type' b) > (type' a) = LT
            | otherwise = foldl (\acc x -> if acc == EQ then cmp x else acc) EQ $ zip a b
                where
                    lengths :: String -> [Int]
                    lengths = reverse . sort . map length . group . sort
                    type' :: String -> Int
                    type' s 
                        | lengths s == [5] = 6 -- Five of a kind
                        | lengths s == [4,1] && 'J' `elem` s = 6 -- Five of a kind
                        | lengths s == [3,2] && 'J' `elem` s = 6 -- Five of a kind
                        | lengths s == [4,1] = 5 -- Four of a kind
                        | lengths s == [3,1,1] && 'J' `elem` s = 5 -- Four of a kind
                        | lengths s == [2,2,1] && (length $ filter (=='J') s) == 2 = 5 -- Four of a kind
                        | lengths s == [3,2] = 4 -- Full house
                        | lengths s == [2,2,1] && (length $ filter (=='J') s) == 1 = 4 -- Full house
                        | lengths s == [3,1,1] = 3 -- Three of a kind
                        | lengths s == [2,1,1,1] && 'J' `elem` s = 3  -- Three of a kind
                        | lengths s == [2,2,1] = 2 -- Two pair
                        | lengths s == [2,1,1,1] = 1  -- Two of a kind
                        | lengths s == [1,1,1,1,1] && 'J' `elem` s = 1 -- Two of a kind
                        | otherwise =  0 -- High card
                    value :: Char -> Int
                    
                    value x
                        | isDigit x = read [x]
                        | x == 'T' = 10
                        | x == 'J' = 1
                        | x == 'Q' = 12
                        | x == 'K' = 13
                        | x == 'A' = 14
                    cmp (a, b)
                        | a == b = EQ
                        | value a > value b = GT
                        | otherwise = LT


main :: IO ()
main = do 
    input <- readFile "input.txt"

    let hands = parseInput $ lines input
    print $ sum . map (\((Hand _ bet), rank) -> rank * bet) $ zip (sort hands) [1..] 