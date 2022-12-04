import Data.List
main :: IO ()
main = do
    inputs <- readFile "input.txt"
    -- print inputs
    let list = convertElf 0 "" inputs
    let sorted = (reverse . sort) list
    print (sorted!!0 + sorted!!1 + sorted!!2)


convertElf :: Int -> String -> String -> [Int]
convertElf n val ['\n'] = [n + (read val :: Int)]
convertElf n val ('\n':'\n':xs) = n + (read val :: Int):(convertElf 0 "" xs)
convertElf n val ('\n':xs) = convertElf ((read val :: Int) + n) "" xs
convertElf n val (x:xs) = convertElf n (val ++ [x]) xs
