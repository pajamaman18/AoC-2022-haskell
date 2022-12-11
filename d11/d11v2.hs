import Data.Char
import Data.List.Split


main :: IO ()
main =
    do
        i <- readFile "input.txt"
        let monkeMoves = map simpMonkey  (chunksOf 7 $ parse "" i)
        let totInspects = do20 20 monkeMoves
        let zeros = take 4 $ [0..]
        -- let insp = foldr (\n acc -> zipWith (+) n acc) zeros totInspects
        let insp = map (\n ->foldr (\a c -> c + (a!!n)) 0 totInspects) [0..3]
        let m1 = maximum insp
        let m2 = maximum $ removeItem m1 insp
        print $ m1 * m2
        print insp
        print totInspects


removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

do20 :: Int -> [(Int,[Int],(Int->Int),Int,Int,Int)] -> [[Int]]
do20 0 _ = []
do20 rem l =  [inspects] ++ do20 (rem-1) (newMoves newmoves l)

    where
        things = doround l
        inspects = map (\(a,_)->a) things
        newmoves = (concat . map (\(_,b)->b)) things

newMoves :: [(Int,Int)] -> [(Int,[Int],(Int->Int),Int,Int,Int)] -> [(Int,[Int],(Int->Int),Int,Int,Int)]
newMoves moves = 
    map (\(num,toInspect,oper,divisor,iftrue,iffalse) -> (num,(map (\(_,n)->n) . filter (\(nr,_)->nr==num)) moves,oper,divisor,iftrue,iffalse))
                                            -- inspcts, new inspects
doround :: [(Int,[Int],(Int->Int),Int,Int,Int)] -> [(Int,[(Int,Int)])]
doround [] = []
doround ((num,toInspect,oper,divisor,iftrue,iffalse):others) =
    (length toInspect, filter (\(nr,worry) -> nr < num) addressMonk) : (doround $ otherMonkeItems addressMonk others)
        where
            newMonk = (map (\val -> val `div` 3) . map oper) toInspect
            addressMonk = map (whereMonk divisor iftrue iffalse) newMonk

whereMonk :: Int -> Int -> Int -> Int -> (Int,Int)
whereMonk divisor t f value
    | value `mod` divisor == 0 = (t,value)
    | otherwise = (f,value)


otherMonkeItems :: [(Int,Int)] -> [(Int,[Int],(Int->Int),Int,Int,Int)] -> [(Int,[Int],(Int->Int),Int,Int,Int)]
otherMonkeItems items = 
    map (\(num,toInspect,oper,divisor,iftrue,iffalse) -> (num,toInspect ++ ((map (\(a,b) -> b) . filter (\(nr,_)-> nr == num)) items),oper,divisor,iftrue,iffalse))

makeOp :: String -> (Int->Int)
makeOp "* old" = (^2)
makeOp str
    | take 1 str == "+" = (+) (read $ drop 2 str)
    | otherwise = (*) (read $ drop 2 str)
                        -- nr  items  operation div T   F
simpMonkey :: [String] -> (Int,[Int],(Int->Int),Int,Int,Int)
simpMonkey l = (
    ((read . take 1) $ drop 7 $ l!!0),
    (map read . map (\c -> filter isNumber c)) ((drop 2 . words) (l!!1)),
    makeOp (drop 23 $ l!!2),
    read $ drop ((length (l!!3)) -2) (l!!3),
    read $ drop ((length (l!!4)) -1) (l!!4),
    read $ drop ((length (l!!5)) -1) (l!!5))

parse :: String -> String -> [String]
parse str ['\n'] = [str]
parse str ('\n':xs) = str: (parse "" xs)
parse str (x:xs) = parse (str++[x]) xs