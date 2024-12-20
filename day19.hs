import Data.List (isPrefixOf)
import Data.Maybe (isJust, mapMaybe)

parse :: String -> ([String], [String])
parse s =
  let a = lines s in
  let b : _ : c = a in
  let d = words $ filter (/= ',') b in
  (d, c)

match :: [String] -> String -> Maybe Int
match dict s = head (match' (replicate (1 + length s) $ Just 0) dict dict s)
  where match' :: [Maybe Int] -> [String] -> [String] -> String -> [Maybe Int]
        match' _ _ _ [] = [Just 1]
        match' (c:cs) _ [] _
          | c == Just 0 = Nothing : cs
          | otherwise = c:cs
        match' (Just c:cs) dict (d:ds) s
          | d `isPrefixOf` s = case (Just c:cs) !! length d of
            Nothing -> match' (Just c:cs) dict ds s
            Just 0 ->
              let l = length d in
              let cache' = match' (drop l (Just c:cs)) dict dict (drop l s) in
              let cache'' = take l (Just c:cs) ++ cache' in
              if isJust $ head cache' then
                let Just c' = head cache' in
                match' (Just (c + c') : tail cache'') dict ds s
              else
                match' cache'' dict ds s
            Just c' -> match' (Just (c + c') : cs) dict ds s
          | otherwise = match' (Just c:cs) dict ds s

main :: IO ()
main = do
  input <- readFile "./input"
  let (dict, lines) = parse input
  let sol1 = length $ filter isJust $ match dict <$> lines
  putStrLn $ "First task: " ++ show sol1
  let sol2 = sum $ mapMaybe (match dict) lines
  putStrLn $ "Second task: " ++ show sol2
