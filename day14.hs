import Data.Maybe (mapMaybe)
import Data.Char (isDigit)

type Vec = (Int, Int)

width = 101
height = 103

parse :: String -> ([Vec], [Vec])
parse s = unzip $ parse' <$> lines s
  where parse' s =
          let [a, b, c, d] = read <$> words (mapMaybe f s) in
          ((a, b), (c, d))
          where f c
                  | isDigit c || c == '-' = Just c
                  | c == ' ' || c == ',' = Just ' '
                  | otherwise = Nothing

move :: Int -> (Vec, Vec) -> Vec
move c ((x, y), (u, v)) = (mod (x + c * u) width, mod (y + c * v) height)

countQuadrants :: [Vec] -> (Int, Int, Int, Int)
countQuadrants = foldr count (0, 0, 0, 0)
  where count (x, y) (a, b, c, d)
          | x < div (width - 1) 2 && y < div (height - 1) 2 = (a + 1, b, c, d)
          | x >= div (width + 1) 2 && y < div (height - 1) 2 = (a, b + 1, c, d)
          | x < div (width - 1) 2 && y >= div (height + 1) 2 = (a, b, c + 1, d)
          | x >= div (width + 1) 2 && y >= div (height + 1) 2 = (a, b, c, d + 1)
          | otherwise = (a, b, c, d)

pretty :: [Vec] -> String
pretty vs = unlines $ prettyY 0
  where prettyY y
          | y == height = []
          | otherwise = prettyX 0 y : prettyY (y + 1)
        prettyX x y
          | x == width = []
          | number == 0 = " " ++ rest
          | otherwise = show number ++ rest
          where number = length $ filter (== (x, y)) vs
                rest = prettyX (x + 1) y

likelyTree :: [Vec] -> Bool
likelyTree vs = count 0 vs
  where count 50 _ = True
        count _ [] = False
        count n ((x, y) : vs)
          | (x, y + 1) `elem` vs = count (n + 1) vs
          | otherwise = count n vs

frame :: [Vec] -> [Vec] -> Int -> IO ()
frame _ _ 10000 = return ()
frame ps ds n =
  if likelyTree ps then do
    putStrLn $ "Frame #" ++ show n
    putStrLn $ pretty ps
    rest
  else
    rest
  where rest = frame (move 1 <$> zip ps ds) ds (n + 1)

main :: IO ()
main = do
  input <- readFile "./input"
  let (a, b) = parse input
  let (c, d, e, f) = countQuadrants $ move 100 <$> zip a b
  let sol1  = c * d * e * f
  putStrLn $ "First task: " ++ show sol1
  -- For my input this prints exactly the frame containing the tree (13s)
  frame a b 0
