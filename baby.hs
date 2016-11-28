--myLast x = last (init x)
--elementAt x y = x !! y
--lenArray array = if null array then 0 else 1 + (lenArray (init array))
--myReverse x = if null x then [] else (last x):(myReverse (init x))
--isPal x = (myReverse x) == x
--consume s = if null s then [] 
--	else (
--		if (not (null (tail s)) && (head s) == (head (tail s))) 
--			then (consume (tail s))
--		else (head s):(consume (tail s)))


repl :: a -> Int -> [a]
repl x n = if n == 0 then [] else x:(repl x (n - 1))

repli :: [a] -> Int -> [a]
repli xs n = if null xs then [] else (repl (head xs) n) ++ (repli (tail xs) n)


 