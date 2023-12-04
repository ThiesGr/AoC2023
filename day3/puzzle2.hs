import Data.Char (isDigit)
import Data.List ((!?), foldl', nub)
import Data.Maybe (isJust, fromJust)
import Data.Monoid ( Sum(Sum, getSum) )

getNumber :: (Int, Int) -> [String] -> Maybe ((Int,Int), String)
getNumber (row, col) schematic
    | not $ isDigit' (row, col) = Nothing
    | otherwise = Just (startOfNumber (row, col), getNumberString $ startOfNumber(row,col))
    where
        isDigit' :: (Int,Int) -> Bool
        isDigit' (row, col) = case schematic !? row of
            Nothing -> False
            Just x -> case x !? col of
                Nothing -> False
                Just x -> isDigit x 
        startOfNumber :: (Int, Int) -> (Int, Int) 
        startOfNumber (row, col)
            | not $ isDigit' (row, col-1) = (row, col)
            | otherwise = startOfNumber (row, col-1)
        getNumberString :: (Int, Int) -> String
        getNumberString (row, col) = takeWhile isDigit $ drop col $ schematic !! row 

getGearRatio :: (Int, Int) -> [String] -> Maybe Int
getGearRatio (row, col) schematic
    | schematic !! row !! col /= '*' = Nothing
    | length connectedNumbers /= 2 = Nothing
    | otherwise = Just (product $ map (\((_,_), n) -> read n) connectedNumbers)
    where
        getNumber' :: (Int,Int) -> Maybe ((Int,Int),String)
        getNumber' (row, col) = getNumber (row, col) schematic
        connectedNumbers' :: [Maybe ((Int,Int),String)]
        connectedNumbers' = 
            [
                getNumber' (row-1, col-1), getNumber' (row-1, col), getNumber' (row-1, col+1),
                getNumber' (row  , col-1)                         , getNumber' (row  , col+1),
                getNumber' (row+1, col-1), getNumber' (row+1, col), getNumber' (row+1, col+1)
            ]
        connectedNumbers :: [((Int,Int), String)]
        connectedNumbers = nub $ map fromJust $ filter isJust connectedNumbers'

solve :: [String] -> Int
solve schematic = getSum $ foldMap (maybe mempty Sum) $ map (\(row, col) -> getGearRatio (row, col) schematic) indices
    where
        nRows = length schematic
        nCols = length $ schematic !! 0
        indices = [(row, col) | row <- [0..nRows-1], col <- [0..nCols-1]]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let schematic = lines input

    print(solve schematic)