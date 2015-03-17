{-# LANGUAGE FlexibleContexts #-}
import Debug.Trace
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Control.Applicative

class MonadTrans t where
    lift :: (Monad m) => m a -> t m a

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
  return x = MaybeT $ return (Just x)
  w >>= f = MaybeT $ do result <- runMaybeT w
                        case result of
                          (Just x) -> runMaybeT $ f x
                          Nothing -> return Nothing

instance MonadTrans MaybeT where
  lift m = MaybeT (liftM Just m)

data StoneType = TypeA | TypePossiblyB | TypeUndetermined deriving (Show, Eq)

bulkWrite :: (MArray a e m, Ix i) =>  a i e -> [(i, e)] -> m [()]
bulkWrite arr xs = sequence [ writeArray arr i e | (i, e) <- xs ]

bulkRead :: (MArray a e m, Ix i) => a i e -> [i] -> m [e]
bulkRead arr is = sequence [ readArray arr i | i <- is ]

mapByIndices :: (MArray a e m, Ix i) => (e -> e) -> a i e -> [i] -> m () 
mapByIndices f arr is = bulkRead arr is >>= bulkWrite arr . zip is . map f >> return ()

subtractLists :: [Int] -> [Int] -> [Int]
subtractLists xs [] = xs
subtractLists [] _ = []
subtractLists a@(x:xs) b@(y:ys)
  | x == y = subtractLists xs b
  | x < y = x:subtractLists xs b
  | otherwise = subtractLists a ys

restIndexes :: Int -> [Int] -> [Int]
restIndexes n = subtractLists [1..n]

amountOfPossiblyB arr is = liftM (length . filter (==TypePossiblyB)) $ bulkRead arr is
                
setTypeAForIndexes :: (MArray a StoneType m) => a Int StoneType -> [Int] -> m Bool
setTypeAForIndexes arr is = do amountOfPossiblyBBefore <- amountOfPossiblyB arr is
                               mapByIndices (const TypeA) arr is
                               amountOfPossiblyBAfter <- amountOfPossiblyB arr is
                               return (amountOfPossiblyBBefore == 0 || amountOfPossiblyBAfter > 0)

setPossiblyBForIndexes :: (MArray a StoneType m) => a Int StoneType -> [Int] -> m Bool
setPossiblyBForIndexes arr is = do mapByIndices (\x -> if x == TypeA then TypeA else TypePossiblyB) arr is
                                   amountOfPossiblyBAfter <- amountOfPossiblyB arr is
                                   return (amountOfPossiblyBAfter > 0)

guard' :: Monad m => Bool -> MaybeT m Bool
guard' True = MaybeT ((return . Just) True)
guard' False = MaybeT (return Nothing)

setIndexesA :: (MArray a StoneType m) => a Int StoneType -> [Int] -> MaybeT m ()
setIndexesA arr is = do lift (setTypeAForIndexes arr is)
                        lift $ (liftM snd . getBounds) arr >>= (setPossiblyBForIndexes arr . (`restIndexes` is))
                        return ()

setIndexesB :: (MArray a StoneType m) => a Int StoneType -> [Int] -> MaybeT m ()
setIndexesB arr is = do lift (setPossiblyBForIndexes arr is) >>= guard'
                        lift $ (liftM snd . getBounds) arr >>= (setTypeAForIndexes arr . (`restIndexes` is))
                        return ()

processMeasure :: (MArray a StoneType m) => Int -> Int -> a Int StoneType -> Bool -> (Int, [Int]) -> MaybeT m Bool
processMeasure a b arr anyB (amount, ss)
  | amount == a * length ss = setIndexesA arr ss >> return anyB
  | amount == a * (length ss - 1) + b = setIndexesB arr ss >> return True
  | otherwise = guard' False >> return undefined

indexesWithTypeB :: (MArray a StoneType m) => a Int StoneType -> m [Int]
indexesWithTypeB arr = do n <- (liftM snd . getBounds) arr
                          xs <- bulkRead arr [1..n]
                          return [ i | (i, x) <- zip [1..n] xs, x == TypePossiblyB]
  
solve :: Int -> Int -> Int -> [(Int, [Int])] -> Maybe [Int]
solve n a b xs = runST $ runMaybeT $ do 
  arr <- lift (newArray (1, n) TypeUndetermined :: ST s (STArray s Int StoneType))                                       
  anyB <- foldM (processMeasure a b arr) False xs                                                                        
  result <- lift $ indexesWithTypeB arr                                                                                  
  guard' $ not (null result && anyB)                                                                                     
  return result

formatOutput :: Maybe [Int] -> String
formatOutput Nothing = "Impossible"
formatOutput (Just []) = "Fail"
formatOutput (Just xs) = ((++"\n") . show . length) xs ++ (unwords . map  show) xs

main = do firstLine <- getLine
          let (n:a:b:k:xs) = (map read . words) firstLine
          entries <- replicateM k getLine
          let parsedEntries = map (map read . words) entries
          putStrLn $ formatOutput $ solve n a b (map (\(x:y:xs) -> (x,xs)) parsedEntries)
