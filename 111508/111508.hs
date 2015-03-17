-- import Data.Array
import Data.List
import Control.Monad.ST
import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Base
import Data.Word
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe
type Graph = Array Int [Int]

             
data SkewHeap a = Empty | SkewNode a (SkewHeap a) (SkewHeap a) deriving (Show)

(+++) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
heap1@(SkewNode x1 l1 r1) +++ heap2@(SkewNode x2 l2 r2) 
  | x1 >= x2    = SkewNode x1 (heap2 +++ r1) l1 
  | otherwise = SkewNode x2 (heap1 +++ r2) l2
Empty +++ heap = heap
heap +++ Empty = heap

addListToHeap :: Ord a => SkewHeap a -> [a] -> SkewHeap a
addListToHeap = foldl (\heap x -> heap +++ SkewNode x Empty Empty)

extractMin Empty = Nothing
extractMin (SkewNode x l r ) = Just (x , l +++ r)
             
bulkRead :: (MArray a e m, Ix i) => a i e -> [i] -> m [e]
bulkRead arr is = sequence [ readArray arr i | i <- is ]

mapByIndex_ :: (MArray a e m) => a Int e -> (e -> e) -> Int -> m ()
mapByIndex_ arr f i = unsafeRead arr i >>= unsafeWrite arr i . f

mapByIndices_' :: (MArray a e m) => a Int e -> (e -> e) -> [Int] -> m ()
mapByIndices_' arr f = mapM_ (mapByIndex_ arr f)

incrementByIndices :: (MArray a e m, Num e) => a Int e -> [Int] -> m ()
incrementByIndices arr = mapByIndices_' arr (+1)

zeroElementsAfterDecrement :: (MArray a e m, Num e, Eq e) => a Int e -> [Int] -> m [Int]
zeroElementsAfterDecrement arr = filterM (\i -> do decremented <- liftM (subtract 1) (unsafeRead arr i)
                                                   unsafeWrite arr i decremented
                                                   return (decremented == 0))

process :: (MArray a e m, Num e, Eq e) => Graph -> a Int e -> SkewHeap Int -> [Int] -> m [Int]
process graph arr heap rez = case extractMin heap of
                               Nothing -> return rez
                               Just (minElem, newHeap) -> do newEntries <- zeroElementsAfterDecrement arr (graph!minElem)
                                                             let newHeap' = addListToHeap newHeap newEntries 
                                                             process graph arr newHeap' (minElem:rez)

topSort :: Graph -> [Int]
topSort graph = runST $ do let upperBound = snd $ bounds graph
                           arr <- newArray (0, upperBound + 1) 0 :: ST s (STUArray s Int Word32)
                           (incrementByIndices arr . concat . elems) graph
                           arrayAsList <- bulkRead arr [1..upperBound]
                           let sources = [ i | (i, e) <- zip [1..] arrayAsList, e == 0]
                           let heap = addListToHeap Empty sources
                           process graph arr heap []

graphFromList :: [[Int]] -> Graph
graphFromList xs = array (1, n) $ zip [1..n] xs
  where n = length xs

mygraph = graphFromList [[2], [], [1, 2], [1, 2, 5], [2], [1, 3, 4, 5]]

myBigGraph n = graphFromList [[i+1..b] | i <- [1..n], let b = min n (i + 1000) ]

main = do (l:ls) <- B.lines `fmap` B.getContents
          let n = fst . fromJust . B.readInt $ l
          entries <- mapM (return . tail . map (fst . fromJust . B.readInt) . B.words) ls
          (putStrLn . unwords . map show . topSort . graphFromList) entries
