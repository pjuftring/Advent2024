import Data.List (uncons, isPrefixOf, transpose)
import GHC.Read (readField)
flipX :: [[a]] -> [[a]]
flipX rs = reverse <$> rs

flipY :: [[a]] -> [[a]]
flipY = reverse

tailOrNothing :: [a] -> [a]
tailOrNothing = maybe [] snd . uncons

cutColumn :: [[a]] -> [[a]]
cutColumn rs = tailOrNothing <$> rs

diag :: [[a]] -> [a]
diag ((r:_):rs) = r : diag (cutColumn rs)
diag _ = []

diagsX rs@((_:_):_) = diag rs : diagsX (cutColumn rs)
diagsX _ = []
diagsY (r:rs) = diag (r:rs) : diagsY rs
diagsY _ = []

diags :: [[a]] -> [[a]]
diags (r:rs) = diagsX (r:rs) ++ diagsY rs
diags _ = []

countXMAS :: [String] -> Int
countXMAS ss = sum $ countXMASLine <$> ss
    where countXMASLine s@(_:cs)
            | isPrefixOf "XMAS" s = 1 + rest
            | otherwise = rest
            where rest = countXMASLine cs
          countXMASLine _ = 0

main :: IO ()
main = do
    input <- readFile "./input"
    let a = lines input
    let b = flipX a
    let c = transpose a
    let d = flipX c
    let e = diags a
    let f = diags b
    let g = diags $ flipY a
    let h = diags $ flipX $ flipY a
    let sol1 = sum $ countXMAS <$> [a, b, c, d, e, f, g, h]
    putStrLn $ "First task: " ++ show sol1
