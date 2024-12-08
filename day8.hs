import Data.List (sort)

type Map = [[Char]]
type Antenna = (Char, Int, Int)
type Pos = (Int, Int)
type AntennaSet = (Char, [Pos])

parseMap :: String -> Map
parseMap = lines

width :: Map -> Int
width = length . head

height :: Map -> Int
height = length

parseAntennas :: Map -> [Antenna]
parseAntennas [] = []
parseAntennas (m:ms) =
  parseAntennasX m ++ map (\(c, x, y) -> (c, x, y + 1)) (parseAntennas ms)
  where parseAntennasX [] = []
        parseAntennasX (c:cs)
          | c == '.' = rest
          | otherwise = (c, 0, 0) : rest
          where rest = map (\(c, x, y) -> (c, x + 1, y)) (parseAntennasX cs)

sortAntennas :: [Antenna] -> [AntennaSet]
sortAntennas = sortAntennas' . sort
  where sortAntennas' [] = []
        sortAntennas' ((c, x, y) : as) = case sortAntennas' as of
          (d, ps) : as | c == d -> (c, (x, y) : ps) : as
          as -> (c, [(x, y)]) : as

getAntinodes :: [Pos] -> [Pos]
getAntinodes [] = []
getAntinodes (p:ps) = concatMap (getAntinodes' p) ps ++ getAntinodes ps
  where getAntinodes' (x, y) (u, v) =
          [(2 * u - x, 2 * v - y), (2 * x - u, 2 * y - v)]

scissor :: Int -> Int -> [Pos] -> [Pos]
scissor width height =
  filter (\(x, y) -> x >= 0 && y >= 0 && x < width && y < height)

unique :: Ord a => [a] -> Int
unique = uniqueSorted . sort
  where uniqueSorted [] = 0
        uniqueSorted [_] = 1
        uniqueSorted (a:b:cs)
          | a == b = uniqueSorted (b:cs)
          | otherwise = 1 + uniqueSorted (b:cs)

getAntinodes2 :: Int -> [Pos] -> [Pos]
getAntinodes2 _ [] = []
getAntinodes2 bound (p:ps) =
  concatMap (getAntinodes' p) ps ++ getAntinodes2 bound ps
  where getAntinodes' (x, y) (u, v) =
          map (\n -> ((n+1) * x - n * u, (n+1) * y - n * v)) [-bound..bound]

main :: IO ()
main = do
  input <- readFile "./input"
  let map = parseMap input
  let w = width map
  let h = height map
  let antennas = parseAntennas map
  let antennaSets = sortAntennas antennas
  let sets = snd <$> antennaSets
  let antinodes = concatMap getAntinodes sets
  let scissored = scissor w h antinodes
  let sol1 = unique scissored
  putStrLn $ "First task: " ++ show sol1
  let antinodes2 = concatMap (getAntinodes2 $ max w h) sets
  let scissored2 = scissor w h antinodes2
  let sol2 = unique scissored2
  putStrLn $ "Second task: " ++ show sol2
