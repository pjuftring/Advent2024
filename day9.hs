import Data.List (singleton)
import Data.Maybe (catMaybes, fromMaybe)

parse :: [Int] -> [Maybe Int]
parse = parseA 0

parseA :: Int -> [Int] -> [Maybe Int]
parseA _[] = []
parseA n (0:is) = parseB n is
parseA n (i:is) = Just n : parseA n (i - 1 : is)

parseB :: Int -> [Int] -> [Maybe Int]
parseB _ [] = []
parseB n (0:is) = parseA (n + 1) is
parseB n (i:is) = Nothing : parseB n (i - 1 : is)

combine :: [Maybe Int] -> [Int] -> [Int]
combine [] bs = bs
combine (Nothing : as) (b:bs) = b : combine as bs
combine (Just a : as) bs = a : combine as bs

checksum :: [Int] -> Int
checksum = checksum' 0
  where checksum' n [] = 0
        checksum' n (i:is) = (n * i) + checksum' (n + 1) is

parse2 :: [Int] -> [(Int, Int)]
parse2 = parse2' 0 0
  where parse2' _ _ [] = []
        parse2' n _ [i] = [(n, i)]
        parse2' n m (0:i2:is) = (n, m) : parse2' (n + 1) 0 is
        parse2' n m (k:i2:is) = parse2' n (m + 1) ((k - 1):i2:is)

combine2 :: (Int, Int) -> [Maybe Int] -> [Maybe Int]
combine2 (id, 0) is = is
combine2 (id, n) [] = replicate n (Just id)
combine2 (id, n) (Nothing : is) = case tryCombine (id, n) (Nothing : is) of
  Just cs -> cs
  Nothing -> Nothing : combine2 (id, n) is
combine2 (id, n) (Just i : is) = Just i : combine2 (id, n) is

delete :: (Int, Int) -> [Maybe Int] -> [Maybe Int]
delete _ [] = []
delete (id, n) (i:is)
  | Just id == i =
      if n > 0 then i : delete (id, n - 1) is else Nothing : delete (id, 0) is
  | otherwise = i : delete (id, n) is

tryCombine :: (Int, Int) -> [Maybe Int] -> Maybe [Maybe Int]
tryCombine (id, 0) is = Just is
tryCombine (id, n) [] = Just $ replicate n (Just id)
tryCombine (id, n) ((Just _) : _) = Nothing
tryCombine (id, n) (Nothing : is) = case tryCombine (id, n - 1) is of
  Just cs -> Just $ Just id : cs
  Nothing -> Nothing

takeMaybes :: Int -> [Maybe Int] -> [Maybe Int]
takeMaybes 0 _ = []
takeMaybes n (Nothing : is) = Nothing : takeMaybes n is
takeMaybes n (Just i : is) = Just i : takeMaybes (n - 1) is

checksum2 :: [Maybe Int] -> Int
checksum2 is = checksum $ fromMaybe 0 <$> is

pretty :: [Maybe Int] -> String
pretty [] = []
pretty (Just i : is) = show i ++ pretty is
pretty (Nothing : is) = '.' : pretty is

main :: IO ()
main = do
  input <- readFile "./input"
  let n = read . singleton <$> head (lines input)
  let a = parse n
  let b = reverse $ catMaybes a
  let c = combine a b
  let d = take (length b) c
  let sol1 = checksum d
  putStrLn $ "First task: " ++ show sol1
  let e = parse2 n
  -- Slow on my machine (36s)
  let f = foldr (\x -> delete x . combine2 x) a e
  let sol2 = checksum2 f
  putStrLn $ "Second task: " ++ show sol2
