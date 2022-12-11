
import Data.List

main :: IO ()
main =
    do
        input <- readFile "input.txt"
        let inputSplit = map formatMoves $ splitContainers "" input
        let size = 25
        let halfSize = (div size 2)
        let halfBoard = take (size-halfSize) $ repeat (take size $ repeat 0)
        -- let bridge = () ++ [(take (size-halfSize) $ repeat 0) ++ [1] ++ (take (size-halfSize-1) $ repeat 0)] ++ (take (size-halfSize) $ repeat (take size $ repeat 0))
        let bridge = halfBoard ++ [(take (size-halfSize-1) $ repeat 0) ++ [1] ++ (take (size-halfSize) $ repeat 0)] ++ halfBoard
        -- let ones = (take 10 $ repeat 1)
        -- let nbridge = (take 2 bridge) ++ [ones] ++ (drop 3 bridge)
        -- print $ length $ bridge
        -- print bridge
        let blank = take size $ repeat (take size $ repeat 0) :: [[Int]]
        
        -- print halfSize
        let start = (size-halfSize-1,halfSize)
        let (_,_,m) = moveH inputSplit ((halfSize,halfSize),(halfSize,halfSize),[])
        print $ length $ nub m
        -- print $ moveplace (4,2) (4,7) nbridge

countPlaces :: [[Int]] -> [Int]
countPlaces a = map sum a

tailDist :: (Int,Int) -> (Int,Int) -> Int
tailDist (headx,heady) (tailx,taily)
    | headx == tailx = abs $ heady - taily
    | heady == taily = abs $ headx - tailx
    | otherwise = max (abs $ heady - taily) (abs $ headx - tailx)


-- d < 2 for part 1
-- part 2 is slightly wrong and i don't know why

pullTail :: (Int,Int) -> (Int,Int) -> (Int,Int)
pullTail (headx,heady) (tailx,taily) 
    | d < 2 = (tailx,taily) 
    | otherwise = moveTail (headx,heady) (tailx,taily) 
        where
            d = tailDist (headx,heady) (tailx,taily) 

moveTail :: (Int,Int) -> (Int,Int) -> (Int,Int)
moveTail (headx,heady) (tailx,taily)
    | headx > tailx && heady == taily = (tailx+1,taily)
    | headx < tailx && heady == taily = (tailx-1,taily)
    | heady > taily && headx == tailx = (tailx,taily+1)
    | heady < taily && headx == tailx = (tailx,taily-1)
    | headx > tailx && heady > taily = (tailx+1,taily+1)
    | headx < tailx && heady > taily = (tailx-1,taily+1)
    | headx > tailx && heady < taily = (tailx+1,taily-1)
    | headx < tailx && heady < taily = (tailx-1,taily-1)

moveH :: [(Char,Int)] ->  ((Int,Int),(Int,Int),[(Int,Int)]) -> ((Int,Int),(Int,Int),[(Int,Int)])
moveH [] ((hx,hy),tail,list) = ((hx,hy),tail,list)
moveH ((dir,dist):rest) ((hx,hy),tail,others)
    | dist == 0 = moveH rest ((hx,hy),tail,others)
    | dir == 'L' = moveH ((dir,dist-1):rest) ((hx-1,hy),pullTail (hx-1,hy) tail,(pullTail (hx-1,hy) tail):others)
    | dir == 'R' = moveH ((dir,dist-1):rest) ((hx+1,hy),pullTail (hx+1,hy) tail,(pullTail (hx+1,hy) tail):others)
    | dir == 'U' = moveH ((dir,dist-1):rest) ((hx,hy-1),pullTail (hx,hy-1) tail,(pullTail (hx,hy-1) tail):others)
    | dir == 'D' = moveH ((dir,dist-1):rest) ((hx,hy+1),pullTail (hx,hy+1) tail,(pullTail (hx,hy+1) tail):others)

formatMoves :: String -> (Char, Int)
formatMoves str = (str!!0, (read . (drop 2)) str)

splitContainers :: String -> String -> [String]
splitContainers str ['\n'] = [str]
splitContainers str ('\n':xs) = str: (splitContainers "" xs)
splitContainers str (x:xs) = splitContainers (str++[x]) xs