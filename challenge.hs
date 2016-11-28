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


untilPalindrome' :: (Num a) => a -> Int
untilPalindrome' x = 

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    let inputs = [let w = words a in (read (last w) :: Int, init w) | a <- (lines input)]
        answers = [getMthElement (fst w) (snd w) | w <- inputs]
    mapM_ putStrLn $ catMaybes answers








