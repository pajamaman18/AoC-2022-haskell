import Data.Char
import Data.List

main :: IO ()
main = 
    do
        inputs <- readFile ".txt"
        let sackList = split "" inputs
        let splitSackList = map (\str -> (slice 0 ((length str) `div` 2) str,slice ((length str) `div` 2) (length str) str)) sackList
        let a = chunk 3 $ map (\(f,b) -> zipWith (+) (makeAppList f) (makeAppList b)) splitSackList
        print $ sum $ map (\[a,b,c] -> getCommon3 0 a b c) a
        -- let a = map (\(f,b) -> getCommon 0 (makeAppList f) (makeAppList b)) splitSackList
        -- print $ sum a
        -- print a

makeAppList :: String -> [Int]
makeAppList = foldr (\c out -> incrListInd (getPriority c) out) [0 | t <- [1..52]]

getCommon :: Int -> [Int] -> [Int] -> Int
getCommon n (x:xs) (y:ys)
    | x > 0 && y > 0 = n + 1
    | otherwise = getCommon (n+1) xs ys

getCommon3 :: Int -> [Int] -> [Int] -> [Int] -> Int
getCommon3 n (x:xs) (y:ys) (z:zs)
    | x > 0 && y > 0 && z > 0 = n + 1
    | otherwise = getCommon3 (n+1) xs ys zs

incrListInd :: Int -> [Int] -> [Int]
incrListInd 0 (x:xs) = (x+1):xs
incrListInd ind (x:xs) = x : (incrListInd (ind-1) xs)

getPriority :: Char -> Int
getPriority a
    | isUpper a = ord (toLower a) - 71
    | otherwise = ord (toUpper a) - 65

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l
  | n > 0 = (take n l) : (chunk n (drop n l))

split :: String -> String -> [String]
split str ['\n'] = [str]
split str ('\n':xs) = str: (split "" xs)
split str (' ':xs) = split str xs
split str (x:xs) = split (str++[x]) xs