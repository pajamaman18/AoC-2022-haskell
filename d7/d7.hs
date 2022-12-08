import Data.Char
data Part = Dir String Int
            | File String Int
            | Cd String
            | Ls
            deriving (Show,Eq)

data Tree = T [Part] [Tree]
            | Null
            deriving (Show)


main :: IO ()
main = 
    do
        inputs <- readFile "t.txt"
        let split = splitContainers "" inputs
        let mapped = map makeData split
        -- print mapped
        let (tDir, subDirs) = getTopDir 1 mapped (drop 1 mapped)
        let subd = splitSubDirs 1 (take 1 subDirs) (drop 1 subDirs)
        -- print tDir
        -- print subd
        let ctree  = cleanTree (T tDir $ makteTrees subd)
        print ctree


updateTree :: Tree -> Tree
updateTree (T vals sub) = updateParts vals sub

updateParts :: [Parts] -> [Parts] -> Tree -> [Parts]
updateParts acc ((Dir name _):xs) trees = 
    where 
        size = sumParts (contentSize name trees) trees 
updateParts acc ((File name size):xs) (T vals sub) = updateParts (acc ++ [File name size]) xs (T vals sub)
updateParts acc (Ls:xs) (T vals sub) = updateParts acc xs (T vals sub)

contentSize :: String -> [Tree] -> Int
contentSize name ((T [dir:rest] st):ts)
    | (Cd name) == dir = sumParts rest
    | otherwise = contentSize name ts

sumParts :: [Parts] -> [Tree] -> Int
sumParts [Null] _ = 0
sumParts [] _ = 0
sumParts ((Dir name):xs) t = (sumparts (contentSize (Dir name) t) t) + sumParts xs
sumParts ((File name size):xs) t = size + sumParts xs t
sumParts (Ls:xs) (T vals sub) = updateParts acc xs (T vals sub)

dirCont :: [Parts] -> Tree -> Tree
dirCont p (T _ t) = (T p t)

cleanTree :: Tree -> Tree
cleanTree Null = Null
cleanTree (T ps tr)
    | null cleaned = Null
    | otherwise = T cleaned (map cleanTree tr)
        where 
            cleaned = (isNotGarb ps)

isNotGarb :: [Part] -> [Part]
isNotGarb (x:xs)
    | x /= (Cd "..") = (x:xs)
    | otherwise = []


makteTrees :: [[Part]] -> [Tree]
makteTrees l
    | (not . null) (l!!0) = map (\s -> T (fst $ funk s) (makteTrees (splitSubDirs 1 (take 1 (snd $ funk s)) (drop 1 (snd $ funk s))))) l
    | otherwise = [Null]
    where
        funk s = getTopDir 1 s (drop 1 s)

splitSubDirs :: Int -> [Part] -> [Part] -> [[Part]]
splitSubDirs _ acc [] = [acc]
splitSubDirs 0 acc x = [acc] ++ splitSubDirs 1 (take 1 x) (drop 1 x)
splitSubDirs depth acc ((Cd name):xs)
    | name == ".." = splitSubDirs (depth - 1) (acc ++ [Cd name]) xs
    | otherwise = splitSubDirs (depth + 1) (acc ++ [Cd name]) xs
splitSubDirs depth acc (x:xs) = splitSubDirs depth (acc ++ [x]) xs

getTopDir :: Int -> [Part] -> [Part] -> ([Part],[Part])
getTopDir n l ((Cd _):xs) = (take n l, drop n l)
getTopDir n l (x:xs) = getTopDir (n+1) l xs
getTopDir n l [] = (l,[])

allDirFilesLs :: [Part] -> [Part] -> [Part]
allDirFilesLs acc ((Cd a):xs) = acc
allDirFilesLs acc (x:xs) = fils
    where
        fils = allDirFilesLs (acc ++ [x]) xs


makeData :: String -> Part
makeData (x:xs)
    | x == '$' = mkCmd (drop 1 xs)
    | otherwise = mkPart (x:xs)

mkPart :: String -> Part
mkPart (x:xs)
    | x == 'd' = Dir (drop 3 xs) 0
    | otherwise = File name size
    where
        (name, size) = splitNameSize "" (x:xs)

splitNameSize :: String -> String -> (String, Int)
splitNameSize acc (x:xs)
    | isNumber x = splitNameSize (acc ++ [x]) xs
    | otherwise= (xs, read (acc ++ [x]) :: Int)

mkCmd :: String -> Part
mkCmd (x:xs)
    | x == 'c' = Cd (drop 2 xs)
    | otherwise = Ls

splitContainers :: String -> String -> [String]
splitContainers str ['\n'] = [str]
splitContainers str ('\n':xs) = str: (splitContainers "" xs)
splitContainers str (x:xs) = splitContainers (str++[x]) xs