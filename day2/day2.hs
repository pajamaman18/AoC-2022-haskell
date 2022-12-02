
-- A = Rock
-- B = Paper
-- C = scissors

main :: IO ()
main = 
    do
        inputs <- readFile "input.txt"
        let splitList = split "" inputs
        -- print $ (sum . (map matchVal)) (map (\[a,b] -> [a,corr b]) splitList)
        print $ (sum . (map matchVal)) (map (\[a,b] -> [a,corr2 a b]) splitList)

matchVal :: String -> Int
matchVal "AA" = 3 + getValue 'A'
matchVal "AB" = 6 + getValue 'B'
matchVal "AC" = 0 + getValue 'C'
matchVal "BA" = 0 + getValue 'A'
matchVal "BB" = 3 + getValue 'B'
matchVal "BC" = 6 + getValue 'C'
matchVal "CA" = 6 + getValue 'A'
matchVal "CB" = 0 + getValue 'B'
matchVal "CC" = 3 + getValue 'C'


corr :: Char -> Char
corr a
    | a == 'X' = 'A'
    | a == 'Y' = 'B'
    | a == 'Z' = 'C'
    | otherwise = a

corr2 :: Char -> Char -> Char
corr2 otherPlayer a
    | a == 'X' = lose otherPlayer
    | a == 'Y' = otherPlayer
    | a == 'Z' = win otherPlayer

win :: Char -> Char
win a
    | a == 'C' = 'A'
    | a == 'B' = 'C'
    | a == 'A' = 'B'

lose :: Char -> Char
lose a
    | a == 'C' = 'B'
    | a == 'B' = 'A'
    | a == 'A' = 'C'

getValue :: Char -> Int
getValue 'A' = 1
getValue 'B' = 2
getValue 'C' = 3 

split :: String -> String -> [String]
split str ['\n'] = [str]
split str ('\n':xs) = str: (split "" xs)
split str (' ':xs) = split str xs
split str (x:xs) = split (str++[x]) xs