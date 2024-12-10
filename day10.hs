import Data.List (sort, singleton)

type Map = [[Int]]
type Pos = (Int, Int)

parseMap :: String -> Map
parseMap = parse . lines
  where parse = map (map (read . singleton))

width :: Map -> Int
width = length . head

height :: Map -> Int
height = length

get :: Map -> Pos -> Maybe Int
get [] _ = Nothing
get (m:ms) (x, 0) = get' m x
  where get' [] _ = Nothing
        get' (i:is) 0 = Just i
        get' (i:is) n = get' is (n - 1)
get (m:ms) (x, y) = get ms (x, y - 1)

collectZeros :: Map -> [Pos]
collectZeros = collect' 0
  where collect' _ [] = []
        collect' v (l:ls) = collectX 0 v l ++ collect' (v + 1) ls
          where collectX _ _ [] = []
                collectX u v (i:is)
                  | i == 0 = (u, v) : rest
                  | otherwise = rest
                  where rest = collectX (u + 1) v is

unique :: Ord a => [a] -> [a]
unique = uniqueSorted . sort
  where uniqueSorted [] = []
        uniqueSorted [a] = [a]
        uniqueSorted (a1:a2:as)
          | a1 == a2 = rest
          | otherwise = a1 : rest
          where rest = uniqueSorted (a2:as)

reached9s :: Int -> Int -> Map -> Bool -> Pos -> [Pos]
reached9s w h m u p = reach 0 w h m u [p]
  where reach 9 w h m u ps = ps
        reach n w h m u ps =
          let f (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] in
          let a = concatMap f ps in
          let b = filter (\(x, y) -> x >= 0 && y >= 0 && x < w && y < h) a in
          let c = filter (\p -> get m p == Just (n + 1)) b in
          let d = if u then unique c else c in
          reach (n + 1) w h m u d

main :: IO ()
main = do
  input <- readFile "./input"
  let m = parseMap input
  let w = width m
  let h = height m
  let z = collectZeros m
  let sol1 = sum (length . reached9s w h m True <$> z)
  putStrLn $ "First task: " ++ show sol1
  let sol1 = sum (length . reached9s w h m False <$> z)
  putStrLn $ "Second task: " ++ show sol1
