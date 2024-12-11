maxSteps :: Int = 75

data Cache = Empty | Node Int Int Cache Cache

lookup :: Int -> Cache -> Maybe Int
lookup _ Empty = Nothing
lookup n (Node k v l r)
  | n == k = Just v
  | n < k = Main.lookup n l
  | otherwise = Main.lookup n r

insert :: Int -> Int -> Cache -> Cache
insert k v Empty = Node k v Empty Empty
insert k v (Node k' v' l r)
  | k == k' = Node k v l r
  | k < k' = Node k' v' (insert k v l) r
  | otherwise = Node k' v' l (insert k v r)

convert :: Int -> Int -> Int
convert c i = (c - 1) + i * maxSteps

countBlink :: Cache -> Int -> Int -> (Cache, Int)
countBlink cache 0 i = (cache, 1)
countBlink cache c 0 = countBlink cache (c - 1) 1
countBlink cache c i =
  case Main.lookup (convert c i) cache of
    Just r -> (cache, r)
    Nothing ->
      let s = show i in
      let l = length s in
      if even l then
        let (sl, sr) = splitAt (div l 2) s in
        let (nl, nr) = (read sl, read sr) in
        let (cache2, resl) = countBlink cache (c - 1) nl in
        let (cache3, resr) = countBlink cache2 (c - 1) nr in
        let res = resl + resr in
        let cache4 = Main.insert (convert c i) res cache3 in
        (cache4, res)
      else
        countBlink cache (c - 1) (i * 2024)

main :: IO ()
main = do
  input <- readFile "./input"
  let nums = read <$> words input
  let sol1 = sum $ snd . countBlink Empty 25 <$> nums
  putStrLn $ "First task: " ++ show sol1
  let sol2 = sum $ snd . countBlink Empty 75 <$> nums
  putStrLn $ "Second task: " ++ show sol2
