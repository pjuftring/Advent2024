type Map = [[Char]]
type Pos = (Int, Int)

width :: Map -> Int
width = length . head

height :: Map -> Int
height = length

new :: Int -> Int -> Map
new _ 0 = []
new n m = replicate n ' ' : new n (m - 1)

readMap :: String -> Map
readMap = lines

isEmptyMap :: Map -> Bool
isEmptyMap = all (all (== ' '))

find :: Map -> Maybe Pos
find m = find' 0 0 m
  where find' _ _ [] = Nothing
        find' x y (m:ms) = case findX x y m of
          Just p -> Just p
          Nothing -> find' x (y + 1) ms
          where findX _ _ [] = Nothing
                findX x y (c:cs)
                  | c /= ' ' = Just (x, y)
                  | otherwise = findX (x + 1) y cs

get :: Map -> Pos -> Maybe Char
get [] _ = Nothing
get (m:ms) (x, 0) = get' m x
  where get' [] _ = Nothing
        get' (c:cs) 0 = Just c
        get' (_:cs) x = get' cs (x - 1)
get (m:ms) (x, y) = get ms (x, y - 1)

set :: Map -> Pos -> Map
set [] _ = []
set (m:ms) (x, 0) = set' m x : ms
  where set' [] _ = []
        set' (c:cs) 0 = 'X' : cs
        set' (c:cs) x = c : set' cs (x - 1)
set (m:ms) (x, y) = m : set ms (x, y - 1)

subtract :: Map -> Map -> Map
subtract [] [] = []
subtract (m:ms) (n:ns) = subtract' m n : Main.subtract ms ns
  where subtract' [] [] = []
        subtract' (c:cs) (d:ds)
          | d == 'X' = ' ' : rest
          | otherwise = c : rest
            where rest = subtract' cs ds

collect' :: Map -> Char -> ([Pos], [Pos], [Pos]) -> [Pos]
collect' m c (as, [], []) = as
collect' m c (as, [], cs) = collect' m c (as, cs, [])
collect' m c (as, b:bs, cs)
  | b `elem` as = collect' m c (as, bs, cs)
  | otherwise =
    let f (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] in
    let d = f b in
    let e = filter (\p -> get m p == Just c) d in
    collect' m c (b:as, bs, cs ++ e)

collect :: Map -> Pos -> Map
collect m p = case get m p of
  Just c ->
    let d = collect' m c ([], [p], []) in
    let e = new (width m) (height m) in
    foldl set e d

nextRegion :: Map -> Map
nextRegion m = case find m of
  Just p -> collect m p
  Nothing -> new (width m) (height m)

forRegions :: Map -> (Map -> a) -> [a]
forRegions m f
  | isEmptyMap m = []
  | otherwise =
    let n = nextRegion m in
    let m' = Main.subtract m n in
    f n : forRegions m' f

area :: Map -> Int
area = foldr (\ m -> (+) (length $ filter (== 'X') m)) 0

data Dir = North | South | West | East deriving (Eq)

fence :: Map -> [(Pos, Dir)]
fence m =
  concat [fence' (x, y) | x <- [0..(width m - 1)], y <- [0..(height m - 1)]]
  where fence' :: Pos -> [(Pos, Dir)]
        fence' p = case get m p of
          Just c | c /= ' ' ->
            let f (x, y) =
                 [((x + 1, y), East),
                  ((x - 1, y), West),
                  ((x, y + 1), South),
                  ((x, y - 1), North)] in
            filter (\(p, _) -> get m p /= Just c) $ f p
          _ -> []

countFence :: Map -> Int
countFence m = length $ fence m

perpendicular :: Dir -> (Int, Int)
perpendicular North = (1, 0)
perpendicular South = (1, 0)
perpendicular West = (0, 1)
perpendicular East = (0, 1)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (u, v) = (x + u, y + v)

countFenceFancy :: Map -> Int
countFenceFancy m =
  let f = fence m in
  fancy f
    where fancy [] = 0
          fancy ((p, d) : fs)
            | (add p $ perpendicular d, d) `elem` fs = rest
            | otherwise = 1 + rest
            where rest = fancy fs

main :: IO ()
main = do
  input <- readFile "./input"
  let map = readMap input
  -- Slowish (7s on my machine)
  let sol1 = sum $ forRegions map (\m -> area m * countFence m)
  putStrLn $ "First task: " ++ show sol1
  -- (Also 7s on my machine)
  let sol2 = sum $ forRegions map (\m -> area m * countFenceFancy m)
  putStrLn $ "Second task: " ++ show sol2
