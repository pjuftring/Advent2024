import Data.List (uncons)

type Map = [[Char]]
type Path = String
type Vec = (Int, Int)

width :: Map -> Int
width = length . head

height :: Map -> Int
height = length

parse :: String -> (Map, Path)
parse s =
  let a = lines s in
  let (bs, c:cs) = break (== "") a in
  (bs, concat cs)

get :: Map -> Vec -> Maybe Char
get [] _ = Nothing
get (m:_) (x, 0) = getX m x
  where getX [] _ = Nothing
        getX (c:_) 0 = Just c
        getX (_:cs) x = getX cs (x - 1)
get (_:ms) (x, y) = get ms (x, y - 1)

set :: Map -> Vec -> Char -> Map
set [] _ _ = []
set (m:ms) (x, 0) c = setX m x c : ms
  where setX [] _ _ = []
        setX (_:ds) 0 c = c:ds
        setX (d:ds) x c = d : setX ds (x - 1) c
set (m:ms) (x, y) c = m : set ms (x, y - 1) c

findRobot :: Map -> Maybe Vec
findRobot m =
  let a = [(x, y) | x <- [0..width m], y <- [0..height m]] in
  let b = filter (\(x, y) -> get m (x, y) == Just '@') a in
  fst <$> uncons b

add :: Vec -> Vec -> Vec
add (x, y) (u, v) = (x + u, y + v)

type Mover = Map -> Vec -> Vec -> Maybe Map

tryMove :: Mover
tryMove m p d = do
  let p' = add p d
  a <- get m p
  b <- get m p'
  m2 <- case b of
    '.' -> Just m
    '#' -> Nothing
    _ -> tryMove m p' d
  let m3 = set m2 p '.'
  let m4 = set m3 p' a
  return m4

charToDirection :: Char -> Vec
charToDirection '^' = (0, -1)
charToDirection '>' = (1, 0)
charToDirection 'v' = (0, 1)
charToDirection '<' = (-1, 0)

applyPath :: Mover -> Map -> Vec -> Path -> Map
applyPath _ m _ [] = m
applyPath mover m p (c:cs) = case mover m p (charToDirection c) of
  Just m' -> applyPath mover m' (add p (charToDirection c)) cs
  Nothing -> applyPath mover m p cs

score :: Map -> Int
score m =
  let a = [(x, y) | x <- [0..width m], y <- [0..height m]] in
  let b = filter (\p -> get m p == Just 'O' || get m p == Just '[') a in
  let c = (\(x, y) -> x + 100 * y) <$> b in
  sum c

expand :: Map -> Map
expand [] = []
expand (m:ms) = expandX m : expand ms
  where expandX [] = []
        expandX ('#':cs) = "##" ++ expandX cs
        expandX ('O':cs) = "[]" ++ expandX cs
        expandX ('.':cs) = ".." ++ expandX cs
        expandX ('@':cs) = "@." ++ expandX cs

tryMove2 :: Mover
tryMove2 m p d = do
  let p' = add p d
  a <- get m p
  b <- get m p'
  m2 <- case b of
    '.' -> Just m
    '#' -> Nothing
    '[' -> do {m' <- tryMove2 m (add p' (1, 0)) d; tryMove2 m' p' d}
    ']' -> do {m' <- tryMove2 m (add p' (-1, 0)) d; tryMove2 m' p' d}
  let m3 = set m2 p '.'
  let m4 = set m3 p' a
  return m4

pretty :: Map -> String
pretty = unlines

main :: IO ()
main = do
  input <- readFile "./input"
  let (map, path) = parse input
  let Just robot = findRobot map
  let map' = applyPath tryMove map robot path
  let sol1 = score map'
  putStrLn $ "First task: " ++ show sol1
  let map2 = expand map
  let Just robot = findRobot map2
  let map2' = applyPath tryMove2 map2 robot path
  let sol2 = score map2'
  putStrLn $ "Second task: " ++ show sol2
