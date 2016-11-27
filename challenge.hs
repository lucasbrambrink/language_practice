import System.Environment (getArgs)

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

main = do
    [inpFile] <- getArgs
    input <- readFile inpFile
    let inputs = map (\x -> read x :: Int) (lines input)
        countedBinaries = map (countOnes' . toBinary') inputs
    mapM_ putStrLn $ map show countedBinaries
