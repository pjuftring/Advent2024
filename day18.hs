import Data.Maybe (isNothing, fromJust)

type Vec = (Int, Int)
data Block = OOB | Corrupted | Safe (Maybe Int)
type Map = [[Block]]
type Path = String

dim :: Int
dim = 71

parseVecs :: String -> [Vec]
parseVecs = parse . lines
  where parse [] = []
        parse (s:ss) =
          let a = (\c -> if c == ',' then ' ' else c) <$> s in
          let b = words a in
          let [c, d] = read <$> b in
          (c, d) : parse ss

newMap :: Map
newMap = replicate dim $ replicate dim $ Safe Nothing

get :: Map -> Vec -> Block
get [] _ = OOB
get (m:_) (x, 0) = getX m x
  where getX [] _ = OOB
        getX (c:_) 0 = c
        getX (_:cs) x = getX cs (x - 1)
get (_:ms) (x, y) = get ms (x, y - 1)

set :: Map -> Vec -> Block -> Map
set [] _ _ = []
set (m:ms) (x, 0) c = setX m x c : ms
  where setX [] _ _ = []
        setX (_:ds) 0 c = c:ds
        setX (d:ds) x c = d : setX ds (x - 1) c
set (m:ms) (x, y) c = m : set ms (x, y - 1) c

applyVecs :: Map -> [Vec] -> Map
applyVecs = foldr (\ v m -> set m v Corrupted)

process :: Map -> Map
process = process' [(0, 0, 0)] []
  where process' [] [] m = m
        process' [] nwl m = process' nwl [] m
        process' ((x, y, s) : wl) nwl m = case get m (x, y) of
          Safe sopt ->
            if isNothing sopt || s < fromJust sopt then
              let a = (\(u, v) -> (x + u, y + v, s + 1))
                   <$> [(1, 0), (-1, 0), (0, 1), (0, -1)] in
              let m' = set m (x, y) $ Safe $ Just s in
              process' wl (nwl ++ a) m'
            else
              process' wl nwl m
          _ -> process' wl nwl m

pretty :: Map -> String
pretty = unlines . pretty'
  where pretty' [] = []
        pretty' (m:ms) = prettyX m : pretty' ms
        prettyX [] = []
        prettyX ((Safe Nothing):bs) = '.' : prettyX bs
        prettyX ((Safe (Just _)):bs) = 'O' : prettyX bs
        prettyX (_:bs) = '#' : prettyX bs

reachedGoal :: Map -> Bool
reachedGoal m = case get m (dim - 1, dim - 1) of
  Safe (Just _) -> True
  _ -> False

find :: Map -> [Vec] -> Maybe Vec
find m vs = find' 0 m vs
  where find' c m vs
          | c == 1 + length vs = Nothing
          | reachedGoal $ process (applyVecs m $ take c vs)
            = find' (c + 1) m vs
          | otherwise = Just (vs !! (c - 1))

main :: IO ()
main = do
  input <- readFile "./input"
  let vs = parseVecs input
  let m = applyVecs newMap $ take 1024 vs
  let m' = process m
  let Safe (Just sol1) = get m' (dim - 1, dim - 1)
  putStrLn $ "First task: " ++ show sol1
  let Just sol2 = find m vs
  -- Slow (27s) but does the job (Cool people would do a binary search)
  putStrLn $ "Second task: " ++ show sol2
