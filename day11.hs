import qualified Data.Map as Map

maxSteps :: Int = 75

type Cache = Map.Map Int Int

convert :: Int -> Int -> Int
convert c i = (c - 1) + i * maxSteps

countBlink :: Cache -> Int -> Int -> (Cache, Int)
countBlink cache 0 i = (cache, 1)
countBlink cache c 0 = countBlink cache (c - 1) 1
countBlink cache c i =
  case Map.lookup (convert c i) cache of
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
        let cache4 = Map.insert (convert c i) res cache3 in
        (cache4, res)
      else
        countBlink cache (c - 1) (i * 2024)

main :: IO ()
main = do
  input <- readFile "./input"
  let nums = read <$> words input
  let sol1 = sum $ snd . countBlink Map.empty 25 <$> nums
  putStrLn $ "First task: " ++ show sol1
  let sol2 = sum $ snd . countBlink Map.empty 75 <$> nums
  putStrLn $ "Second task: " ++ show sol2
