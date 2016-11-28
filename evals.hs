import System.Environment (getArgs)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	-- print your output to stdout
	maps x = if null x then [] else (head x):(maps (tail x))
	x <- mapM_ putStrLn $ lines input
