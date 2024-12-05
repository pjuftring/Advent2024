import Data.Maybe (mapMaybe)
import Data.List (sortBy)

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x (a:as)
  | x == a = [] : rest
  | otherwise = case rest of
    [] -> [[a]]
    (b:bs) -> (a:b) : bs
  where rest = split x as

type Rule = (Int, Int)
type Sequence = [Int]

parseRules :: [String] -> [Rule]
parseRules rs = (\r -> let [a, b] = read <$> split '|' r in (a, b)) <$> rs

parseSeqs :: [String] -> [Sequence]
parseSeqs ss = fmap read . split ',' <$> ss

middleElement :: [a] -> Maybe a
middleElement []  = Nothing
middleElement as = Just $ as !! div (length as) 2

find :: Eq a => [a] -> a -> Maybe Int
find [] _ = Nothing
find (a:as) b
  | a == b = Just 0
  | otherwise = (+ 1) <$> find as b

order :: [Rule] -> Sequence -> Int -> Int -> Ordering
order rs s a b
  | (a, b) `elem` rs = LT
  | (b, a) `elem` rs = GT
  | otherwise = case (find s a, find s b) of
    (Just m, Just n) -> compare m n

repair :: [Rule] -> Sequence -> Sequence
repair rs s = sortBy (order rs s) s

main :: IO ()
main = do
  input <- readFile "./input"
  let [rules', seqs'] = split [] $ lines input
  let (rules, seqs) = (parseRules rules', parseSeqs seqs')
  let oldAndNew = zip seqs $ repair rules <$> seqs
  let getCorrectMiddles (old, new)
       | old == new = middleElement new
       | otherwise = Nothing
  let sol1 = sum $ mapMaybe getCorrectMiddles oldAndNew
  putStrLn $ "First task: " ++ show sol1
  let getRepairedMiddles (old, new)
       | old /= new = middleElement new
       | otherwise = Nothing
  let sol2 = sum $ mapMaybe getRepairedMiddles oldAndNew
  putStrLn $ "Second task: " ++ show sol2
