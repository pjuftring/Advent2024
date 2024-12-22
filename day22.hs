import Data.Bits (xor)
import Data.Maybe (catMaybes)

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune n = mod n 16777216

secret :: Int -> [Int]
secret n =
  let a = prune $ mix (n * 64) n in
  let b = prune $ mix (div a 32) a in
  let c = prune $ mix (b * 2048) b in
  n : secret c

collectSecrets :: [Int] -> [[Int]]
collectSecrets l = collect <$> l
  where collect n =
          let a = take 2001 $ secret n in
          let b = (`mod` 10) <$> a in
          b

numberAfterChange :: (Int, Int, Int, Int) -> [Int] -> Maybe Int
numberAfterChange (a, b, c, d) (e:f:g:h:i:js)
  | a == (f - e) && b == (g - f) && c == (h - g) && d == (i - h) = Just i
  | otherwise = numberAfterChange (a, b, c, d) (f:g:h:i:js)
numberAfterChange _ _ = Nothing

sumForChange :: (Int, Int, Int, Int) -> [[Int]] -> Int
sumForChange c s =
  let a = numberAfterChange c <$> s in
  let b = catMaybes a in
  sum b

setAt :: [a] -> Int -> a -> [a]
setAt (a:as) 0 b = b:as
setAt (a:as) x b = a : setAt as (x - 1) b

best :: [[Int]] -> Int
best ss =
  let a = [(x, y, u, v) | x <- [-9..9],
                          y <- [-9..9],
                          u <- [-9..9],
                          v <- [-9..9]] in
  let b = filter (\(x, y, u, v) ->
       inBounds [x + y, x + y + u, x + y + u + v, y + u, y + u + v, u + v]) a in
  best' 0 b
    where inBounds [] = True
          inBounds (n:ns) = -9 <= n && n <= 9 && inBounds ns
          best' n [] = n
          best' n (c:cs) =
            let m = sumForChange c ss in
            if m > n then best' m cs else best' n cs

main :: IO ()
main = do
  input <- readFile "./input"
  let sol1 = sum ((\n -> secret n !! 2000) . read <$> lines input)
  putStrLn $ "First task: " ++ show sol1
  let a = collectSecrets $ read <$> lines input
  -- _Horribly_ slow (78m) but the potentially more performant version led to
  -- the usual Haskell memory leaks for me
  let sol2 = best a
  putStrLn $ "Second task: " ++ show sol2