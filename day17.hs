import Data.Bits (xor)
import Data.Char (isDigit)

parse :: String -> ([Int], Int, Int, Int)
parse s =
  let [la, lb, lc, _, lp] = lines s in
  let [la', lb', lc', lp'] = filter (\c -> isDigit c || c == ',') <$> [la, lb, lc, lp] in
  let lp'' = read <$> words ((\c -> if c == ',' then ' ' else c) <$> lp') in
  (lp'', read la', read lb', read lc')

combo :: Int -> Int -> Int -> Int -> Int
combo 4 a _ _ = a
combo 5 _ b _ = b
combo 6 _ _ c = c
combo n _ _ _ = n

-- still not in my prelude
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(a:_) !? 0 = Just a
(_:as) !? n = as !? (n - 1)

adv :: Int -> Int -> Int -> Int -> Int
adv op a b c = div a (2 ^ combo op a b c)

bxl :: Int -> Int -> Int
bxl = xor

bst :: Int -> Int -> Int -> Int -> Int
bst op a b c = mod (combo op a b c) 8

jnz :: Int -> Int -> Int -> Int
jnz op 0 n = n + 2
jnz op _ n = op

bxc :: Int -> Int -> Int
bxc = xor

outval :: Int -> Int -> Int -> Int -> Int
outval op a b c = mod (combo op a b c) 8

compute :: [Int] -> Int -> Int -> Int -> [Int]
compute p a b c = compute' p 0 a b c
  where compute' p n a b c = case (p !? n, p !? (n + 1)) of
          (_, Nothing) -> []
          (Just 0, Just op) -> compute' p (n + 2) (adv op a b c) b c
          (Just 1, Just op) -> compute' p (n + 2) a (bxl op b) c
          (Just 2, Just op) -> compute' p (n + 2) a (bst op a b c) c
          (Just 3, Just op) -> compute' p (jnz op a n) a b c
          (Just 4, Just op) -> compute' p (n + 2) a (bxc b c) c
          (Just 5, Just op) -> outval op a b c : compute' p (n + 2) a b c
          (Just 6, Just op) -> compute' p (n + 2) a (adv op a b c) c
          (Just 7, Just op) -> compute' p (n + 2) a b (adv op a b c)

pretty :: [Int] -> String
pretty = init . tail . show

octal :: [Int] -> Int
octal = octal' . reverse
  where octal' [] = 0
        octal' (i:is) = i + 8 * octal' is

increment :: [Int] -> Int -> [Int]
increment [] _ = []
increment (i:is) 0 = (i + 1) : is
increment (i:is) n = i : increment is (n - 1)

find :: [Int] -> Int
find p = find' 0 (take (length p) (1 : repeat 0)) p
  where find' n as p
          | n == length p = octal as
          | compute p (octal as) 0 0 !! (length p - 1 - n)
             == p !! (length p - 1 - n) = find' (n + 1) as p
          | otherwise = find' n (increment as n) p

main :: IO ()
main = do
  input <- readFile "./input"
  let (p, a, b, c) = parse input
  let sol1 = pretty $ compute p a b c
  putStrLn $ "First task: " ++ sol1
  let sol2 = find p
  putStrLn $ "Second task: " ++ show sol2
