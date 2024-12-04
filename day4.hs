import Data.List (uncons, transpose)
import GHC.Read (readField)
flipX :: [[a]] -> [[a]]
flipX rs = reverse <$> rs

flipY :: [[a]] -> [[a]]
flipY = reverse

tailOrNothing :: [a] -> [a]
tailOrNothing = maybe [] snd . uncons

cutColumn :: [[a]] -> [[a]]
cutColumn rs = tailOrNothing <$> rs

linePatternChecks :: String -> String -> Bool
linePatternChecks ('_':ps) (_:ss) = linePatternChecks ps ss
linePatternChecks (p:ps) (s:ss) = p == s && linePatternChecks ps ss
linePatternChecks (_:_) [] = False
linePatternChecks [] _ = True


check :: [String] -> [String] -> Bool
check (g:gs) (p:ps) = linePatternChecks p g && check gs ps
check [] (_:_) = False
check _ _ = True

count :: [String] -> [String] -> Int
count (g:gs) pattern = countX (g:gs) + count gs pattern
  where countX ([]:_) = 0
        countX grid
          | check grid pattern = 1 + rest grid
          | otherwise = rest grid
          where rest grid = countX $ cutColumn grid
count [] _ = 0


main :: IO ()
main = do
    input <- readFile "./input"
    let l = lines input
    let pattern = ["XMAS"]
    let pattern_diag = ["X___", "_M__", "__A_", "___S"]
    let sol1 = sum $ count l <$> [pattern, flipX pattern, transpose pattern, transpose $ flipX pattern, pattern_diag, flipX pattern_diag, flipY pattern_diag, flipX $ flipY pattern_diag]
    putStrLn $ "First task: " ++ show sol1
