import Data.List
import Control.Monad (replicateM)
 
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs 
merge [] ys = ys 
merge a@(x:xs) b@(y:ys)
  | x == y = x:merge xs ys
  | x < y = x:merge xs b
  | otherwise = y:merge a ys

substract :: [Int] -> [Int] -> [Int]
substract xs [] = xs
substract [] _ = []
substract a@(x:xs) b@(y:ys)
  | x == y = substract xs b
  | x < y = x:substract xs b
  | otherwise = substract a ys

setIntersect :: [Int] -> [Int] -> [Int]
setIntersect _ [] = []
setIntersect [] _ = []        
setIntersect a@(x:xs) b@(y:ys)
  | x == y = x:setIntersect xs ys
  | x < y = setIntersect xs b
  | otherwise = setIntersect a ys
  
checkEntriesTotals :: Int -> Int -> [(Int, [Int])] -> Maybe [(Int, [Int])]
checkEntriesTotals a b xs
  | all (\(amount, es) -> length es * a == amount || (length es - 1) * a + b == amount) xs = Just xs
  | otherwise = Nothing

removeFakeStones :: [[Int]] -> [Int] -> [[Int]]
removeFakeStones entryList fakeStoneList = map (`substract` fakeStoneList) entryList

checkEntriesNotEmpty :: [[Int]] -> Maybe [[Int]]
checkEntriesNotEmpty xs
  | any null xs = Nothing
  | otherwise = Just xs

findEntriesIntersection :: [[Int]] -> Maybe [Int]
findEntriesIntersection [] = Just []
findEntriesIntersection (x:xs) = Just $ foldl setIntersect x xs

stripWeight :: (Int, [Int]) -> [Int]
stripWeight (a, xs) = xs

stripWeightPair :: ([(Int, [Int])], [(Int, [Int])]) -> ([[Int]], [[Int]])
stripWeightPair (fs, rs) = (map stripWeight fs, map stripWeight rs)

solve :: Int -> Int -> Int -> [(Int, [Int])] -> Maybe [Int]
solve n a b xs = do 
  checkEntriesTotals a b xs                                                                                                    
  let (fakeEntries, realEntries) = stripWeightPair $ partition (\(amount, xs) -> amount == length xs * a) xs                   
  entries <- checkEntriesNotEmpty $ removeFakeStones realEntries (foldl merge [] fakeEntries)                                  
  let notMentioned = substract [1..n] (foldl merge [] fakeEntries)                                                             
  findEntriesIntersection (notMentioned:entries)

formatOutput :: Maybe [Int] -> String
formatOutput (Just []) = "Fail"
formatOutput (Just xs) = show (length xs) ++ "\n" ++ unwords (map show xs)
formatOutput Nothing = "Impossible"

main = do firstLine <- getLine
          let (n:a:b:k:xs) = (map read . words) firstLine
          entries <- replicateM k getLine
          let parsedEntries = map (map read . words) entries
          putStrLn $ formatOutput $ solve n a b (map (\(x:y:xs) -> (x,xs)) parsedEntries)
