main :: IO ()


main = do
    inputs <- readFile "input.txt"
    -- print inputs
    let list = convertElf 0 "" inputs
    let highest =  maximum list
    let list2 = removeItem highest list
    let highest2 = maximum list2
    let list3 = removeItem highest2 list2
    let highest3 = maximum list3
    print (highest + highest2 + highest3)

removeItem :: Int -> [Int] -> [Int]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

convertElf :: Int -> String -> String -> [Int]
convertElf n val ['\n'] = [n + (read val :: Int)]
convertElf n val ('\n':'\n':xs) = n + (read val :: Int):(convertElf 0 "" xs)
convertElf n val ('\n':xs) = convertElf ((read val :: Int) + n) "" xs
convertElf n val (x:xs) = convertElf n (val ++ [x]) xs