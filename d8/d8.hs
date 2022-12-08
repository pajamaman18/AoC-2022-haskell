import Data.Char(digitToInt)

main :: IO ()
main =
    do
        inputs <- readFile "input.txt"
        let split = splitContainers "" inputs
        let inted = [map digitToInt out | out <- split]
        -- let inted = split!!0!!0
        -- print $ checkTree 4 4 inted
        -- print $ reverse $ getRow 4 inted
        -- let ind =  (length ((getRow 4 inted)))-4-1
        -- print $ checkLeft ind 0 (reverse $ getRow 4 inted)
        let cout = [out | i <- [0..((length inted)-1)], out <- [row | j <- [0..((length (inted!!i))-1)], row <- [checkTree i j inted]]]
        print $ sum cout
        let scenic = [out | i <- [0..((length inted)-1)], out <-[row | j <- [0..((length (inted!!i))-1)], row <- [scenicValue i j inted]]]
        -- print $ scenicValue 1 2 inted
        -- print $ inted!!1!!2
        print $ maximum scenic


checkTree :: Int -> Int -> [[Int]] -> Int
checkTree i j array
    | left || right || up || down = 1
    | otherwise = 0
        where
            left = checkLeft j (array!!i!!j) ((getRow i array))
            right = checkLeft ((length ((getRow i array)))-j-1) (array!!i!!j) (reverse ((getRow i array)))
            down = checkLeft i (array!!i!!j) (getCol j array)
            up = checkLeft ((length (getCol j array)) - i-1) (array!!i!!j) (reverse (getCol j array))

-- getHeight :: Int -> Int -> [[Int]] -> Int
-- getHeight i j a = a!!i!!j

checkLeft :: Int -> Int -> [Int] -> Bool
checkLeft _ _ [] = True
checkLeft 0 _ _ = True
checkLeft index height (x:xs)
    | x >= height = False
    | otherwise = checkLeft (index-1) height xs

scenicValue :: Int -> Int -> [[Int]] -> Int
scenicValue r c array =  left * right * up * down
    where
        left = scenicLeft (array!!r!!c) 0 ((reverse . take (c)) (getRow r array))
        right = scenicLeft (array!!r!!c) 0 (drop (c+1) (getRow r array))
        up = scenicLeft (array!!r!!c) 0 ((reverse . take (r)) (getCol c array))
        down = scenicLeft (array!!r!!c) 0 (drop (r+1) (getCol c array))

scenicLeft :: Int -> Int -> [Int] -> Int
scenicLeft h n [] = n
scenicLeft h n (x:xs)
    | x >= h = (n+1)
    | otherwise = scenicLeft h (n+1) xs

getRow :: Int -> [[Int]] -> [Int]
getRow row matrix = matrix!!row

getCol :: Int -> [[Int]] -> [Int]
getCol col matrix = [out | r <- [0..rows], out <- [matrix!!r!!col]]
    where
        rows = (length matrix) - 1

splitContainers :: String -> String -> [String]
splitContainers str ['\n'] = [str]
splitContainers str ('\n':xs) = str: (splitContainers "" xs)
splitContainers str (x:xs) = splitContainers (str++[x]) xs