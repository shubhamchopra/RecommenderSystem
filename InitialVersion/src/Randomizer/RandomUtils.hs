{-# LANGUAGE BangPatterns #-}
module Randomizer.RandomUtils (
    shuffleListIO
    , shuffleVectorIO
    , randomSplitIO
  ) where

import qualified System.Random as R
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector as DV
import Control.Monad (forM_, liftM)

import Control.Monad.ST
import Data.STRef

shuffleVectorST :: R.RandomGen t => DV.Vector a -> t -> (DV.Vector a, t)
shuffleVectorST xs gen = runST $ do
  g <- newSTRef gen
  let n = DV.length xs
      randomRST lohi = do
        (a, s') <- liftM (R.randomR lohi) (readSTRef g)
        writeSTRef g s'
        return a
      copyVectorToMutable xs' = do
        v <- GM.new n
        forM_ [0 .. (n - 1)] $ \i ->
          GM.write v i (xs' DV.! i)
        return v

  v <- copyVectorToMutable xs
  forM_ [0 .. (n - 1)] $ \i -> do
    j <- randomRST (i, n - 1)
    GM.swap v i j
  ret <- DV.unsafeFreeze v
  g' <- readSTRef g
  return (ret, g')

shuffleListIO :: [a] -> IO (DV.Vector a)
shuffleListIO = shuffleVectorIO . DV.fromList

shuffleVectorIO :: DV.Vector a -> IO (DV.Vector a)
shuffleVectorIO xs = R.getStdRandom (shuffleVectorST xs)

randomSplitIO :: Int -> DV.Vector a -> IO (DV.Vector a, DV.Vector a)
randomSplitIO n xs = liftM (DV.splitAt n) (shuffleVectorIO xs)


