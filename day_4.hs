import Data.Text (pack, splitOn, unpack)
import Data.Set (toList, fromList)
import Control.Monad (msum)
import Data.Maybe (isJust, isNothing)

type Combinations = [[Int]]
type Board = [[Int]]
type Column = [Int]
type Row = [Int]

parseDrawNumbers :: String -> [Int]
parseDrawNumbers x = fmap ( read . unpack) (splitOn (pack ",") (pack x))

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards (x:xs)
    | null x = fmap (fmap read . words) (takeWhile (not . null) xs) : parseBoards xs
    | otherwise = parseBoards xs

getRows :: Board -> [Row]
getRows = id

getColumns :: Board -> [Column]
getColumns [[],[],[],[],[]] = []
getColumns x = map head x : getColumns (fmap (drop 1) x)

getCombinations :: Board -> Combinations
getCombinations board = getRows board ++ getColumns board

mkUniq :: Ord a => [a] -> [a] -- from stackoverflow
mkUniq = toList . fromList

calculateBingoScore :: [Int] -> Board -> Maybe Int
calculateBingoScore [] _ = Nothing
calculateBingoScore numbers board
    | bingo = Just (sum (mkUniq $ concat filtered) * last numbers)
    | otherwise = Nothing
    where
        combinations = getCombinations board
        filtered = fmap (filter (`notElem` numbers)) combinations
        bingo = foldl (\acc x -> null x || acc ) False filtered

-- part 1
winBingo :: [Board] -> [Int] -> [Int] -> Maybe Int
winBingo [] _ _ = Nothing
winBingo _ _ [] = Nothing
winBingo boards drawnNumbers (x:xs)
    | isJust score = score
    | otherwise = winBingo boards (drawnNumbers ++ [x]) xs
    where
        f = calculateBingoScore drawnNumbers
        score = msum $ fmap f boards

-- part 2
loseBingo :: [Board] -> [Int] -> [Int] -> Maybe Int
loseBingo [] _ _ = Nothing
loseBingo _ _ [] = Nothing
loseBingo boards drawnNumbers (x:xs)
    | length filteredBoards == 1 = winBingo filteredBoards (drawnNumbers ++ [x]) xs
    | otherwise = loseBingo filteredBoards (drawnNumbers ++ [x]) xs
    where
        f = calculateBingoScore drawnNumbers
        filteredBoards = filter (isNothing . f) boards

main = do
    content <- readFile "input.txt"
    let (n:ns) = lines content
    let numbers = parseDrawNumbers n
    let boards = parseBoards ns
    let winingScore = winBingo boards [] numbers
    let losingScore = loseBingo boards [] numbers
    return losingScore