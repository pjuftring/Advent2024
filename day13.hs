import Data.Char (isDigit)

type Vec = (Int, Int)
type Config = (Vec, Vec, Vec)

split :: String -> [String]
split [] = [""]
split (c:cs)
  | c == ',' = "" : split cs
  | otherwise =
    let (s:ss) = split cs in
    (c:s) : ss

parse :: String -> [Config]
parse s =
  let a = filter (/= "") $ lines s in
  let b = filter (\c -> isDigit c || c == ',') <$> a in
  let c = concatMap split b in
  let d = read <$> c in
  collect d
  where collect [] = []
        collect (i1:i2:i3:i4:i5:i6:is) =
          ((i1, i2), (i3, i4), (i5, i6)) : collect is

best :: Config -> Int
best ((px, py), (qx, qy), (rx, ry)) =
  let b = div (py * rx - px * ry) (py * qx - px * qy) in
  let a = div (rx - b * qx) px in
  let sx = a * px + b * qx in
  let sy = a * py + b * qy in
  if sx == rx && sy == ry then
    3 * a + 1 * b
  else 
    0

main :: IO ()
main = do
  input <- readFile "./input"
  let cs = parse input
  let sol1 = sum $ best <$> cs
  putStrLn $ "First task: " ++ show sol1
  let f (p, q, (rx, ry)) = (p, q, (10000000000000 + rx, 10000000000000 + ry))
  let cs2 = f <$> cs
  let sol2 = sum $ best <$> cs2
  putStrLn $ "Second task: " ++ show sol2
