predOnList :: (a -> a -> Bool) -> [a] -> Bool
predOnList p (a:b:cs) = p a b && predOnList p (b:cs)
predOnList _ _ = True

isAscending :: [Int] -> Bool
isAscending = predOnList (<=)

isDescending :: [Int] -> Bool
isDescending = predOnList (>=)

hasRightSpeed :: [Int] -> Bool
hasRightSpeed = predOnList (\x y -> abs (x - y) >= 1 && abs (x - y) <= 3)

isSafe :: [Int] -> Bool
isSafe s = hasRightSpeed s && (isAscending s || isDescending s)

dropElement :: [a] -> Int -> [a]
dropElement (a:as) 0 = as
dropElement (a:as) n = a : dropElement as (n-1)
dropElement [] _ = []

variants :: [a] -> [[a]]
variants as = dropElement as <$> take (length as) [0..]

isSafeWithVariants :: [Int] -> Bool
isSafeWithVariants s = any isSafe (variants s)

main :: IO ()
main = do
    input <- readFile "./input"
    let l = lines input
    let sol1 = length $ filter (\s -> isSafe $ read <$> words s) l
    putStrLn $ "First task: " ++ show sol1
    let sol2 = length $ filter (\s -> isSafeWithVariants $ read <$> words s) l
    putStrLn $ "Second task: " ++ show sol2
