import Data.Char (isDigit)
import Data.List ((!?))
import Data.Monoid ( Sum(Sum, getSum) )

hasAdjacentSymbol :: (Int,Int) -> [String] -> Bool
hasAdjacentSymbol (row, col) schematic = 
    hasSymbol (row-1) (col-1) || hasSymbol (row-1) col || hasSymbol (row-1) (col+1) ||
    hasSymbol  row    (col-1) ||                          hasSymbol  row    (col+1) ||
    hasSymbol (row+1) (col-1) || hasSymbol (row+1) col || hasSymbol (row+1) (col+1)
    where
        isSymbol c = c /= '.' && (not $ isDigit c)
        hasSymbol r c = case schematic !? r of
            Nothing -> False
            Just x -> case x !? c of 
                Nothing -> False
                Just x -> isSymbol x

getNumber :: (Int, Int) -> [String] -> Maybe Int
getNumber (row, col) schematic
    | isDigit' (row, col-1) = Nothing
    | not $ numberHasAdjectSymbols (row, col) = Nothing
    | otherwise = Just $ read (takeWhile isDigit $ drop col $ schematic !! row)
    where
        isDigit' (row, col) = case schematic !! row !? col of
            Nothing -> False
            Just x -> isDigit x
        numberHasAdjectSymbols (row, col) 
            | not $ isDigit' (row, col) = False
            | hasAdjacentSymbol (row, col) schematic = True
            | otherwise = numberHasAdjectSymbols (row, col+1)

solve :: [String] -> Int
solve schematic = getSum . foldMap (maybe mempty Sum) $ map (\(row, col) -> getNumber (row, col) schematic) indices
    where
        nRows = length schematic
        nCols = length $ schematic !! 0
        indices = [(row, col) | row <- [0..nRows-1], col <- [0..nCols-1]]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let schematic = lines input
    let nRows = length schematic

    print(solve schematic)