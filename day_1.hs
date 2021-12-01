import System.IO

filterIncreases [] = []
filterIncreases [_] = []
filterIncreases (x : y : xs)
  | y > x = y : filterIncreases (y : xs)
  | otherwise = filterIncreases (y : xs)

mapToThreeMeasurement [] = []
mapToThreeMeasurement [_] = []
mapToThreeMeasurement [_, _] = []
mapToThreeMeasurement (x : y : z : xs) = (x + y + z) : mapToThreeMeasurement (y : z : xs)

main = do
  content <- readFile "input.txt"
  let depths = map (read :: String -> Int) $ lines content
  -- part 1 increases
  let increases = length $ filterIncreases depths
  -- part 2 increases
  let increases2 = length $ filterIncreases $ mapToThreeMeasurement depths
  putStrLn ("Part 1: " ++ show increases ++ ", Part 2: " ++ show increases2)