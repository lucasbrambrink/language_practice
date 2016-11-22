quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = 
    let smallSort = quicksort [a | a <- xs, a <= x]
        bigSort = quicksort [a | a <- xs, a > x]
    in smallSort ++ [x] ++ bigSort


-- Merge Sort
mergeArray :: (Ord a) => [a] -> [a] -> [a]
mergeArray [] [] = []
mergeArray [x] [y] = if x <= y then [x, y] else [y, x]
mergeArray (x:xs) [] = x:xs
mergeArray [] (y:ys) = y:ys
mergeArray (x:xs) (y:ys) = 
    if x <= y then x:(mergeArray xs (y:ys)) else y:(mergeArray (x:xs) ys)

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort (x:xs) =
    let a = splitAt (quot (length (x:xs)) 2) (x:xs)
        b = mergesort (fst a)
        c = mergesort (snd a)
    in mergeArray b c
