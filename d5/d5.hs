import Data.Char
import Data.List

main :: IO ()
main = do
    inputs <- readFile "input.txt"
    -- print $ take 8 $ splitContainers "" inputs
    let stacks = (map (\stack -> filter (\p -> not(null p)) stack)) $ transpose $ simplifyCrates $ map splitIntoList (take 8 $ splitContainers "" inputs)
    let moves = map (simplifyMoves "") (drop 10 $ splitContainers "" inputs)
    -- print stacks
    print moves
    let moved = moveAround stacks moves
    print moved
    print $ (concat . (map head)) moved


moveAround :: [[String]] -> [[Int]] -> [[String]]
moveAround boxes [] = boxes
moveAround boxes (x:xs) = moveAround (changeStacks x boxes) xs

changeStacks :: [Int] -> [[String]] -> [[String]]
changeStacks [amount,from,to] list
    | to < from = take to list ++ [st2] ++ drop (to+1) (take from list) ++ [st1] ++ drop (from+1) list
    | otherwise = take from list ++ [st1] ++ drop (from+1) (take to list) ++ [st2] ++ drop (to+1) list
    where
        (st1,st2) = move2Stacks [amount+1,from,to] list


-- for part2 add `reverse $` before take amount 
move2Stacks :: [Int] -> [[String]] -> ([String],[String])
move2Stacks [amount,from,to] l = (drop amount (stack1), (take amount stack1) ++ (stack2))
    where
        stack1 = l!!from
        stack2 = l!!to

-- changeIndex

simplifyMoves :: String -> String -> [Int]
simplifyMoves v [] = [(read v)-1]
simplifyMoves val (x:xs)
    | isNumber x = simplifyMoves (val ++ [x]) xs
    | (not . null) val = [(read val :: Int)-1] ++ simplifyMoves "" xs
    | otherwise = simplifyMoves "" xs

simplifyCrates :: [[String]] -> [[String]]
simplifyCrates = (map . map) (filter (\c -> not(isSpace c || c == '[' || c == ']'))) 

splitIntoList :: String -> [String]
splitIntoList [] = []
splitIntoList l = [take 3 l] ++ splitIntoList (drop 4 l)

splitContainers :: String -> String -> [String]
splitContainers str ['\n'] = [str]
splitContainers str ('\n':xs) = str: (splitContainers "" xs)
splitContainers str (x:xs) = splitContainers (str++[x]) xs
