import Data.List
import Control.Arrow ((&&&))

data Bit = Zero | One  deriving (Show, Eq, Bounded, Ord, Enum)
type Binary = [Bit]

class Opposite a where
    opposite :: a -> a

instance Opposite Bit where
    opposite Zero = One
    opposite One = Zero

toBit :: Char -> Bit
toBit x
    | x == '0' = Zero
    | x == '1' = One

toBinary :: String -> Binary
toBinary = fmap toBit

invert :: Binary -> Binary
invert = fmap opposite

mostFrequent :: Binary -> Bit
mostFrequent = snd . maximum . map (length &&& head) . group . sort

mostfrequency :: Binary -> Int
mostfrequency = fst . maximum . map (length &&& head) . group . sort

removeBits :: Int -> [Binary] -> [Binary]
removeBits _ [] = []
removeBits n (x:xs) = newBinary : removeBits n xs
    where
        newBinary = drop n x

toMostCommonBit :: [Binary] -> Binary
toMostCommonBit [] = []
toMostCommonBit (x:xs) = y : toMostCommonBit xs
    where
        (y:ys) = x

toMostCommonBits :: [Binary] -> [Binary]
toMostCommonBits [] = []
toMostCommonBits (x:xs) = if length x == 1 then [binary] else binary : toMostCommonBits (removeBits 1 (x:xs))
    where
        binary = head x : toMostCommonBit xs

bintodec :: Binary -> Int
bintodec = foldl (\y x -> fromEnum x + 2*y) 0

oxygenRating :: Int -> [Binary] -> Binary
oxygenRating _ [x] = x
oxygenRating n xs = oxygenRating (n + 1) filtered
    where
        binaries = removeBits n xs
        bitCriteria = head (mostFrequent <$> toMostCommonBits binaries)
        filtered = [x | x <- xs, x !! n == bitCriteria]

co2Rating :: Int -> [Binary] -> Binary
co2Rating _ [x] = x
co2Rating n xs = co2Rating (n + 1) filtered
    where
        binaries = removeBits n xs
        bitCriteria = opposite $ head (mostFrequent <$> toMostCommonBits binaries)
        filtered = [x | x <- xs, x !! n == bitCriteria]

main = do
    content <- readFile "input.txt"
    let binaries = toBinary <$> lines content
    let gamma = mostFrequent <$> toMostCommonBits binaries
    let epsilon = invert gamma
    let gammaDecimal = bintodec gamma
    let epsilonDecimal = bintodec epsilon
    let oxygen = bintodec $ oxygenRating 0 binaries
    let co2 = bintodec $ co2Rating 0 binaries
    return (oxygen * co2)
