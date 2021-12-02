type Horizontal = Int
type Depth = Int
type Aim = Int
type Position = (Horizontal, Depth, Aim)

multiplyPosition :: Position -> Int
multiplyPosition (x, y, _) = x * y

forward :: Int -> Position -> Position
forward n (x, y, z) = (x + n, y + (n * z), z)

down :: Int -> Position -> Position
down n (x, y, z) = (x, y, z + n)

up :: Int -> Position -> Position
up n (x, y, z) = (x, y, z - n)

move :: Position -> String  -> Position
move position command = newPosition
        where
            (direction:amount:_) = words command
            n = read amount :: Int
            newPosition = case direction of
                "forward" -> forward n position
                "down" -> down n position
                "up" -> up n position

main = do
    content <- readFile "input.txt"
    let commands = lines content
    let position = foldl move (0,0,0) commands
    putStrLn ("Part 2: " ++ show (multiplyPosition position))