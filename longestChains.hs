import Data.List
import System.Environment (getArgs)

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

-- Parse input
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
