import Data.List

type Position = (Int, Int)
type Direction = (Int, Int)
type Map = [String]

-- does not seem to exist in my Haskell version??
(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (a:as) 0 = Just a
(!?) (_:as) n = as !? (n - 1)

get :: Map -> Position -> Maybe Char
get [] _ = Nothing
get (m:ms) (x, y)
  | x < 0 || y < 0 = Nothing
  | y == 0 = m !? x
  | otherwise = get ms (x, y - 1)

set :: Map -> Position -> Map
set [] _ = []
set (m:ms) (x, y)
  | x < 0 || y < 0 = m:ms
  | y == 0 = setAt m x : ms
  | otherwise = m : set ms (x, y - 1)
  where setAt [] _ = []
        setAt (c:cs) 0 = 'X' : cs
        setAt (c:cs) n = c : setAt cs (n - 1)

add :: Num a => (a, a) -> (a, a) -> (a, a)
add (x, y) (u, v) = (x + u, y + v)

turn :: Direction -> Direction
turn (x, y) = (-y, x)

move :: Map -> Position -> Direction -> Maybe (Position, Direction)
move m p d =
  let new_p = add p d in
  case get m new_p of
    Just '#' -> move m p (turn d)
    Just _ -> Just (new_p, d)
    Nothing -> Nothing

path :: Map -> Position -> Direction -> [Position]
path m p d = case move m p d of
  Just (new_p, new_d) -> p : path m new_p new_d
  Nothing -> [p]

unique :: Ord a => [a] -> Int
unique = count . sort
  where count [] = 0
        count [_] = 1
        count (c:c':cs)
          | c == c' = rest
          | otherwise = 1 + rest
          where rest = count (c':cs)

extract :: String -> (Map, Maybe Position)
extract = extractRec . lines
  where extractRec [] = ([], Nothing)
        extractRec (m:ms) = case elemIndex '^' m of
          Just n -> (m:ms, Just (n, 0))
          Nothing -> let (ms', p) = extractRec ms in
                     (m:ms', fmap (\(x, y) -> (x, y + 1)) p)

checkLoop :: Map -> Position -> Direction -> Bool
checkLoop m p d
  | Just 'X' == get m p = True
  | otherwise = let new_m = set m p in
                case move new_m p d of
                  Just (new_p, new_d) -> checkLoop new_m new_p new_d
                  Nothing -> False

mapSize :: Map -> Int
mapSize = foldr ((+) . length) 0

allMaps :: Map -> Position -> [Map]
allMaps [] _ = [[]]
allMaps (m:ms) (x, y) =
  fmap (: ms) (allMapsLine m ms (x, y)) ++ fmap (m :) (allMaps ms (x, y - 1))
  where allMapsLine [] _ _ = []
        allMapsLine (m:ms) rest (x, y)
          | m == '#' || (x, y) == (0, 0) =
            fmap (m :) (allMapsLine ms rest (x - 1, y))
          | otherwise = ('#':ms) : fmap (m :) (allMapsLine ms rest (x - 1, y))

main :: IO ()
main = do
  input <- readFile "./input"
  let (map, Just pos) = extract input
  let sol1 = unique $ path map pos (0, -1)
  putStrLn $ "First task: " ++ show sol1
  -- Slow (50s on my machine) but does the job
  let sol2 = countLoops (mapSize map) pos (allMaps map pos) 0
  putStrLn $ "Second task: " ++ show sol2
  where countLoops _ _ [] _ = 0
        countLoops size pos (m:ms) n
          | length (take size (path m pos (0, -1))) == size = 1 + rest
          | otherwise = rest
          where rest = countLoops size pos ms (n+1)
