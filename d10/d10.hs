import Data.List.Split

main :: IO ()
main =
    do
        input <- readFile "input.txt"
        let instr = map simp $ parse "" input
        -- print instr
        let mem = run 1 instr
        print $ part1 $ mem
        print $ getBlock $ part2 0 mem
        -- print $ mem !!40

-- split screen into multiple lines
getBlock :: String -> [String]
getBlock = chunksOf 40

-- check if dist between center and position is `< 2` if yes add '#' otherwise '.'
part2 :: Int -> [Int] -> String
part2 _ [] = []
part2 pos (x:xs)
    | abs (pos-x) < 2 = '#': part2 ((pos+1) `mod` 40) xs
    | otherwise = '.' : part2 ((pos+1) `mod` 40) xs

-- very cursed solution for part 1
part1 :: [Int] -> Int
part1 l = (l!!19)*20 + (l!!59)*60 + (l!!99)*100 + (l!!139)*140 + (l!!179)*180 + (l!!219)*220

-- calculate all values of x per cycle
run :: Int -> [Int] -> [Int]
run val [] = [val]
run prev (x:xs) = add x ++ run (prev+x) xs 
    where
        add 0 = [prev]
        add val = prev : [prev]

simp :: String -> Int
simp str
    | length str == 4 = 0
    | otherwise = (read . drop 5) str

parse :: String -> String -> [String]
parse str ['\n'] = [str]
parse str ('\n':xs) = str: (parse "" xs)
parse str (x:xs) = parse (str++[x]) xs