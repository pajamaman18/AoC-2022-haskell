

main :: IO ()
main =
    do
        -- 4 for part 1, 14 for part 2
        let n = 14
        inputs <- readFile "input.txt"
        print $ afterN n (take n inputs) (drop n inputs)

afterN :: Int -> String -> String -> Int
afterN n _ [] = n
afterN n list rem
    | diff list = n
    | otherwise = afterN (n+1) ((drop 1 list) ++ (take 1 rem)) (drop 1 rem)

diff :: String -> Bool
diff [] = True
diff (x:xs) = (all (/=x) xs )&& diff xs
