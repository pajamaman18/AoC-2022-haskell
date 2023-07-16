import Data.List.Split
-- import Data.Text

main :: IO ()
main =
    do
        inp <- readFile "input.txt"
        let coupleList = chunksOf 2 $ map makList $ (filter (not . null) . parse "") inp

        print $ makList [] coupleList



checkLists :: [Int] -> [Int] -> Bool
checkLists a [] = False
checkLists [] a = True
checkLists (x:xs) (y:ys)
    | x < y = True
    | x > y = False
    | otherwise = checkLists xs ys

makList :: [Int] -> String -> [[Int]]
makList _ [] = []
makList acc (']':xs) = [(map read . splitOn ",") acc ] ++ makList [] xs
makList acc (x:xs)
    | x == '[' = acc : makList [] xs
    | otherwise = acc ++ [[(read $ x:"")]]

parse :: String -> String -> [String]
parse str ['\n'] = [str]
parse str ('\n':xs) = str: (parse "" xs)
parse str (x:xs) = parse (str++[x]) xs