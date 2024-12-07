import Data.Maybe (mapMaybe)

apply :: Bool -> [Int] -> Int -> [Int]
apply _ [] _ = []
apply False (i:is) n = i + n : i * n : apply False is n
apply True (i:is) n = i + n : i * n : read (show i ++ show n) : apply True is n

results :: Bool -> [Int] -> [Int]
results b (i:is) = foldl (apply b) [i] is

process :: Bool -> String -> Maybe Int
process b s =
  let (res' : nums') = words s in
  let res = read $ init res' in
  let nums = read <$> nums' in
  if res `elem` results b nums then Just res else Nothing

main :: IO ()
main = do
  input <- readFile "./input"
  let sol1 = sum $ mapMaybe (process False) $ lines input
  putStrLn $ "First task: " ++ show sol1
  -- Somewhat slow: 7s on my machine
  let sol2 = sum $ mapMaybe (process True) $ lines input
  putStrLn $ "Second task: " ++ show sol2
