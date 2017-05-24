import System.Environment (getArgs)
import Data.List
import Data.Maybe 

fib' :: Int -> Int
fib' 0 = 0
fib' 1 = 1
fib' x = (fib' (x-1)) + (fib' (x-2))

-- Binary converter
toBinary' :: Int -> Int
toBinary' 0 = 0
toBinary' base10 = 
    let bestBase2 = last $ takeWhile (\x -> 2^x <= base10) [0,1..]
    in 10^bestBase2 + toBinary' (base10 - 2^bestBase2)

countOnes' :: (Num a, Ord a) => Int -> Int
countOnes' x = 
    let digits = [snd a | a <- (zip [1..] (show x))]
    in length $ filter (=='1') digits

-- Prime
isPrime' :: Int -> Bool
isPrime' 1 = True
isPrime' x = not $ any (\y -> (mod x y) == 0) [2..(x-1)]
    

showPrimeUntil :: Int -> String
showPrimeUntil x = 
    let primeList = map show $ filter isPrime' [2..x]
    in intercalate "," primeList

-- Mth 
getMthElement :: Int -> [a] -> Maybe a
getMthElement m list = 
    if m > length list then Nothing
        else Just $ reverse list !! (m - 1)

isPalindrome' :: (Ord a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [x] = True
isPalindrome' (x:xs) = 
    if not $ x == last xs then False 
    else isPalindrome' (init xs)

nextIteration' :: Int -> Int
nextIteration' x =
    let revNum = read (reverse (show x)) :: Int
    in x + revNum

untilPalindrome' :: [Int] -> [Int]
untilPalindrome' [] = []
untilPalindrome' xs =
    let lastNum = head xs
    in if isPalindrome' (show lastNum) then xs
    else untilPalindrome' $ (nextIteration' lastNum):xs

findPal' :: Int -> String
findPal' x = 
    let palindrome = untilPalindrome' [x]
    in unwords $ map show [length palindrome - 1, head palindrome]

-- Cocktail sort
bubbleForward' :: (Ord a) => [a] -> [a]
bubbleForward' [] = []
bubbleForward' [x] = [x]
bubbleForward' (x:xs) = 
    let y = head xs
        z = tail xs
    in if x < y then x:(bubbleForward' $ y:z)
      else y:(bubbleForward' $ x:z)

bubbleBackward' :: (Ord a) => [a] -> [a]
bubbleBackward' [] = []
bubbleBackward' [x] = [x]
bubbleBackward' (x:xs) = 
    let y = head xs
        z = tail xs
    in if x > y then x:(bubbleBackward' $ y:z)
      else y:(bubbleBackward' $ x:z)

bubbleBackwards' :: (Ord a) => [a] -> [a]
bubbleBackwards' xs = reverse $ bubbleBackward' $ reverse xs

cocktailsort' :: (Ord a) => Int -> [a] -> [a]
cocktailsort' _ [] = []
cocktailsort' _ [x] = [x]
cocktailsort' 0 xs = xs
cocktailsort' i xs =
    let nextPass = bubbleBackwards' $ bubbleForward' xs
    in cocktailsort' (i - 1) nextPass

-- Combinations
combinations' :: (Ord a) => [a] -> Int -> [[a]]
combinations' xs 1 = [[x] | x <- xs]
combinations' xs num = 
    let prev = combinations' xs (num - 1)
    in [(y:x) | x <- prev, y <- xs, y `notElem` x]


combs' :: (Ord a) => [a] -> Int -> [[a]]
combs' xs num = nub $ combinations' xs num


factorial' :: Int -> Int
factorial' 1 = 1
factorial' x = x * factorial' (x - 1)

-- n! / k!(n - k)!
numCombs' :: Int -> Int -> Int
numCombs' n k = (factorial' n) `div` (factorial' n) * (factorial' (n - k))

--Word chains
addLinks' :: [String] -> [String] -> [[String]]
addLinks' chain wrds =
    let link = head chain
        otherWords = filter (\x -> x /= link) wrds
    in [newLink:chain | newLink <- (filter (\x -> last link == head x) wrds), newLink `notElem` chain]


applyNextChain' :: [[String]] -> [String] -> [[String]]
applyNextChain' chains wrds = [c | t <- [addLinks' x wrds | x <- chains], c <- t]

untilLongest' :: [[String]] -> [String] -> [[String]]
untilLongest' chains wrds = 
    let nextIter = applyNextChain' chains wrds
    in if null nextIter then chains
        else if (length $ head nextIter) > (length $ head chains)
        then untilLongest' nextIter wrds else nextIter

longestChain' :: [String] -> String
longestChain' wrds = 
    let longestChain = head $ untilLongest' [[x] | x <- wrds] wrds
    in if (length longestChain) == 1 then "None"
        else show $ length longestChain


-- Parsing by comma
splitAlong' :: (Char -> Bool) -> String -> [String]
splitAlong' predicate "" = []
splitAlong' predicate xs =
    let split = break predicate xs
    in (fst split):(splitAlong' predicate $
        let z = snd split in if null z then [] else tail z)

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    let inputs = [splitAlong' (==',') a | a <- (lines input)]
        chainLengths = [longestChain' line | line <- inputs]
    mapM_ putStrLn $ chainLengths








