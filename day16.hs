import Data.List (sort)
import Data.Maybe (isNothing, fromJust)

type Vec = (Int, Int)
type Block = Maybe [(Maybe Int, [Vec])]
type Map = [[Block]]
type Path = String

width :: Map -> Int
width = length . head

height :: Map -> Int
height = length

parse :: String -> Map
parse s = parse' <$> lines s
  where parse' [] = []
        parse' ('#':cs) = Nothing : parse' cs
        parse' (_:cs) =
          Just [(Nothing, []),
                (Nothing, []),
                (Nothing, []),
                (Nothing, [])] : parse' cs

getPosOf :: String -> Char -> Maybe Vec
getPosOf s c = getPosOf' 0 0 c $ lines s
  where getPosOf' x y c [] = Nothing
        getPosOf' x y c (s:ss) = case getPosOfX x y c s of
          Just v -> Just v
          Nothing -> getPosOf' x (y + 1) c ss
          where getPosOfX x y c [] = Nothing
                getPosOfX x y c (d:ds)
                  | c == d = Just (x, y)
                  | otherwise = getPosOfX (x + 1) y c ds

get :: Map -> Vec -> Block
get [] _ = Nothing
get (m:_) (x, 0) = getX m x
  where getX [] _ = Nothing
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

numToDir :: Int -> Vec
numToDir 0 = (1, 0)
numToDir 1 = (0, 1)
numToDir 2 = (-1, 0)
numToDir 3 = (0, -1)

setAt :: [a] -> Int -> a -> [a]
setAt (a:as) 0 b = b:as
setAt (a:as) x b = a : setAt as (x - 1) b

add :: Vec -> Vec -> Vec
add (x, y) (u, v) = (x + u, y + v)

unique :: Ord a => [a] -> [a]
unique = uniqueSorted . sort
  where uniqueSorted [] = []
        uniqueSorted [a] = [a]
        uniqueSorted (a1:a2:as)
          | a1 == a2 = rest
          | otherwise = a1 : rest
          where rest = uniqueSorted (a2:as)

processMap :: Map -> Vec -> Map
processMap m v = process m [(v, 0, 0, [])] []
  where process m [] [] = m
        process m [] nwl = process m nwl []
        process m ((v, d, s, l) : wl) nwl = case get m v of
          Just a ->
            if isNothing (fst (a !! d)) || s <= fromJust (fst (a !! d)) then
              let l' = if
                isNothing (fst (a !! d)) || s < fromJust (fst (a !! d)) then
                    v : l
                  else
                    unique (v : (l ++ snd (a !! d))) in
              let b = setAt a d (Just s, l') in
              let m' = set m v (Just b) in
              let c = [(v, mod (d - 1) 4, s + 1000, l'),
                       (v, mod (d + 1) 4, s + 1000, l'),
                       (add v (numToDir d), d, s + 1, l')] in
              process m' wl (nwl ++ c)
            else
              process m wl nwl
          Nothing -> process m wl nwl

main :: IO ()
main = do
  input <- readFile "./input"
  let map = parse input
  let Just start = getPosOf input 'S'
  let Just end = getPosOf input 'E'
  let map' = processMap map start
  let (s, l) = minimum (fromJust (get map' end))
  let sol1 = fromJust s
  putStrLn $ "First task: " ++ show sol1
  let sol2 = length l
  -- 5s on my machine
  putStrLn $ "Second task: " ++ show sol2
