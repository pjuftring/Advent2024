import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.List (tails, isPrefixOf)

splitAfterChar :: String -> Char -> Maybe (String, String)
splitAfterChar (c:cs) c'
    | c == c' = Just ([c], cs)
    | otherwise = do
        (sl, sr) <- splitAfterChar cs c'
        return (c:sl, sr)
splitAfterChar [] _ = Nothing

assert :: Bool -> Maybe ()
assert True = Just ()
assert False = Nothing

getInt :: String -> Maybe Int
getInt s
    | all isDigit s && not (null s) = Just (read s)
    | otherwise = Nothing

giveProduct :: String -> Maybe Int
giveProduct s = do
    (a, b) <- splitAfterChar s '('
    assert (a == "mul(")
    (c, d) <- splitAfterChar b ','
    left <- getInt (init c)
    (e, _) <- splitAfterChar d ')'
    right <- getInt (init e)
    return (left * right)

collectWithSwitch :: Bool -> [String] -> [Int]
collectWithSwitch _ [] = []
collectWithSwitch b (s:ss)
    | "do()" `isPrefixOf` s = collectWithSwitch True ss
    | "don't()" `isPrefixOf` s = collectWithSwitch False ss
    | b = maybe rest (:rest) (giveProduct s)
    | otherwise = rest
    where rest = collectWithSwitch b ss
    
main :: IO ()
main = do
    input <- readFile "./input"
    let sol1 = sum $ mapMaybe giveProduct $ tails input
    putStrLn $ "First task: " ++ show sol1
    let sol2 = sum $ collectWithSwitch True $ tails input
    putStrLn $ "Second taks: " ++ show sol2
