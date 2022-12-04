import Data.List
import Data.Maybe

main :: IO ()
main = do
    inputs <- readFile "input.txt"
    let sepWords = split "" inputs
    let split = map (\x -> (take (fromJust $ elemIndex ',' x) x ,drop (1+ (fromJust $ elemIndex ',' x)) x )) sepWords
    let intSplit = map (\(a,b) -> (splitChar '-' a,splitChar '-' b)) split
    -- print $ sum $ map between intSplit
    print $ sum $ map overlap intSplit


splitChar :: Char -> String -> (Int,Int)
splitChar ch str = (read $ take (fromJust $ elemIndex ch str) str ,read $ drop (1+ (fromJust $ elemIndex ch str)) str )

overlap :: ((Int,Int),(Int,Int)) -> Int
overlap ((a,b),(c,d))
    | a >= c && b <= d = 1
    | c >= a && d <= b = 1
    | b >= c && a <= c = 1
    | d >= a && c <= a = 1
    | otherwise = 0

between :: ((Int,Int),(Int,Int)) -> Int
between ((a,b),(c,d))
    | a >= c && b <= d = 1
    | c >= a && d <= b = 1
    | otherwise = 0

split :: String -> String -> [String]
split str ['\n'] = [str]
split str ('\n':xs) = str: (split "" xs)
split str (' ':xs) = split str xs
split str (x:xs) = split (str++[x]) xs
