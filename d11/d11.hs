import Data.List.Split
import Data.Char

main :: IO ()
main =
    do
        input <- readFile "t.txt"
        let l = parse "" input
        let monkeys = map (\l -> filter (\s -> (not . null) s) l) (take 7 l : chunksOf 7 (drop 7 l))
        let tthrows = do20 20 $ map simpMonkey monkeys
        let throws = filter (\c -> (not . null) c) tthrows
        -- print throws
        let asda = [(length t, snd (t!!0)) | t <- throws]
        let splitInd = map (\n -> filter (\(am,nr) -> n == nr) asda) [0..3]
        let p1 = map (\t -> (sum . map (\(a,_)-> a)) t) splitInd
        -- let part1 = [out | t <- splitInd, out <- (map sum . map (\(a,b) -> a)) t]
        -- let am = (map length . map (\nr -> filter (\(_,n) -> nr == n) throws)) [0..3]
        let fthis = chunksOf 4 $ (map id . map length) throws
        -- print fthis
        let m1 = maximum p1
        let m2 = maximum $ removeItem m1 p1
        print $ m1 * m2
        -- print $ chunksOf 4 $ map (\(a,_)->a) asda
        print splitInd
        -- print $ throws!!0


removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

do20 :: Int -> [(Int,[Int],String,Int,Int,Int)] -> [[(Int,Int)]]
do20 left list
    | left > 0 = inspected ++ (do20 (left-1) $ afterInspect newVals (map (\(worry,toInspect,op,divisor,iftrue,iffalse) -> (worry,[],op,divisor,iftrue,iffalse)) list))
    | otherwise = []
    where
        inspected = map (\(a,b)-> a) r
        newVals = (concat . map (\(a,b)-> b)) r
        r = doround list

sortApe :: [(Int,Int)] -> [[Int]]
sortApe l = map (\nr -> (map (\(w,_)->w) . filter (\(w,n) -> nr == n)) l) [0..3]

doround :: [(Int,[Int],String,Int,Int,Int)] -> [([(Int,Int)],[(Int,Int)])]
doround [] = []
doround ((num,toInspect,oper,divisor,iftrue,iffalse):others) =
    (map (\v -> (v,num)) toInspect, filter (\(w,n) -> n <= num) apeDone) : doround (afterInspect apeDone others)
    where
        apeDone = (map (inspectElem oper divisor iftrue iffalse) toInspect)

afterInspect :: [(Int,Int)] ->  [(Int,[Int],String,Int,Int,Int)] ->  [(Int,[Int],String,Int,Int,Int)]
afterInspect thrown = 
    map (\(nr,items,op,d,t,f) -> (nr,items ++ ((map (\(worry, to) -> worry) . filter (\(worry, to) -> to == nr)) thrown),op,d,t,f))

            --  oper,   div,   true,   fal,   wrry, (wrry,mnkynr)
inspectElem :: String -> Int -> Int -> Int -> Int -> (Int,Int)
inspectElem "* old" divisor true fal worry
    | (v `mod` divisor) == 0 = (v,true)
    | otherwise = (v,fal)
        where 
            v = (worry * worry) `div` 3
          
inspectElem operation divisor true fal worry
    | take 1 operation == "+" = 
        if plusval `mod` divisor == 0
            then (plusval,true)
            else (plusval,fal)
    | otherwise =
        if mulval `mod` divisor == 0
            then (mulval,true)
            else (mulval,fal)
        where
            modif = (read . drop 2) operation
            plusval = (worry + modif) `div` 3
            mulval = (worry * modif) `div` 3

                        -- no, item, op,    div,true,fal
simpMonkey :: [String] -> (Int,[Int],String,Int,Int,Int)
simpMonkey l = (
    ((read . take 1) $ drop 7 $ l!!0),
    (map read . map (\c -> filter isNumber c)) ((drop 2 . words) (l!!1)),
    drop 23 $ l!!2,
    read $ drop ((length (l!!3)) -2) (l!!3),
    read $ drop ((length (l!!4)) -1) (l!!4),
    read $ drop ((length (l!!5)) -1) (l!!5))

parse :: String -> String -> [String]
parse str ['\n'] = [str]
parse str ('\n':xs) = str: (parse "" xs)
parse str (x:xs) = parse (str++[x]) xs