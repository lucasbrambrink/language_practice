-- Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = 
    let smallSort = quicksort [a | a <- xs, a <= x]
        bigSort = quicksort [a | a <- xs, a > x]
    in smallSort ++ [x] ++ bigSort


-- Merge Sort
merge' :: (Ord a) => [a] -> [a] -> [a]
merge' [] [] = []
merge' [x] [y] = if x <= y then [x, y] else [y, x]
merge' (x:xs) [] = x:xs
merge' [] (y:ys) = y:ys
merge' (x:xs) (y:ys) = 
    if x <= y then x:(merge' xs (y:ys)) else y:(merge' (x:xs) ys)

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort (x:xs) =
    let bifurcated = splitAt (quot (length (x:xs)) 2) (x:xs)
        lowerHalf = mergesort (fst bifurcated)
        upperHalf = mergesort (snd bifurcated)
    in merge' lowerHalf upperHalf

-- Bubble sort
bubble' :: (Ord a) => [a] -> [a]
bubble' [] = []
bubble' [x] = [x]
bubble' (x:xs) = 
    let y = head xs
        z = tail xs
    in if x < y then y:(bubble' $ x:z)
      else x:(bubble' $ y:z)

bubblesort :: (Ord a) => [a] -> [a]
bubblesort [] = []
bubblesort x = 
    let bubbledThrough = bubble' x
    in (last bubbledThrough):(bubblesort $ init bubbledThrough)


-- Cocktail sort
bubbleDir' :: (Ord a) => [a] -> [a]
bubbleDir' [] = []
bubbleDir' [x] = [x]
bubbleDir' (x:xs) = 
    let y = head xs
        z = tail xs
    in if x < y then x:(bubbleDir' $ y:z)
      else y:(bubbleDir' $ x:z)

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














