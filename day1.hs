import Data.List (sort)

toPair :: String -> (Int, Int)
toPair s = let [a, b] = read <$> words s in (a, b)

sortPair :: Ord a => ([a], [a]) -> ([a], [a])
sortPair (a, b) = (sort a, sort b)

dist :: (Int, Int) -> Int
dist (a, b) = abs $ a - b

main :: IO ()
main = do
    input <- readFile "./input"
    let (left, right) = unzip $ toPair <$> lines input
    let sol1 = sum $ fmap dist $ uncurry zip $ sortPair (left, right)
    putStrLn $ "First taks: " ++ show sol1
    let sol2 = sum [i | i <- left, j <- right, i == j]
    putStrLn $ "Second task: " ++ show sol2
