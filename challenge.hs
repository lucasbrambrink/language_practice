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


main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    let inputs = [let w = words a in (read (last w) :: Int, map (\x -> read x :: Int) $ (init $ init w)) | a <- (lines input)]
        answers = map unwords $ [let sorted = cocktailsort' (fst t) (snd t) in map show sorted | t <- inputs]
    mapM_ putStrLn $ answers








