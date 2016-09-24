{-# LANGUAGE BangPatterns #-}

module Randomizer.RandomUtils (
    shuffleListIO
    , shuffleVectorIO
    , randomSplitIO
    , shuffleVectorIO'
  ) where

import qualified System.Random as R
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector as DV
import Control.Monad (forM_, liftM)

import Control.Monad.ST
import Data.STRef

import qualified Data.Map.Strict as M

swap :: M.Map Int Int -> Int -> Int -> M.Map Int Int
swap !imap !i !j = newMap
  where (Just iV) = M.lookup i imap
        (Just jV) = M.lookup j imap
        m' = M.insert i jV imap
        newMap = M.insert j iV m'

accFunc :: R.RandomGen t => Int -> (M.Map Int Int, t) -> Int -> (M.Map Int Int, t)
accFunc !n (!m, !g) !i = (m', g')
  where (j, g') = R.randomR (i, n-1) g
        m' = swap m i j

initMap :: Int -> M.Map Int Int
initMap n = M.fromList [(i, i) | i <- [0 .. (n-1)]]

shuffleVector :: R.RandomGen t => DV.Vector a -> t -> (DV.Vector a, t)
shuffleVector xs gen = (xs', gen')
  where
    xs' = map' `seq` DV.backpermute xs $ DV.fromList $ M.elems map'
    (map', gen') = DV.foldl' (accFunc n) (initMap n, gen) (DV.generate n id)
    n = DV.length xs

shuffleVectorST :: R.RandomGen t => DV.Vector a -> t -> (DV.Vector a, t)
shuffleVectorST !xs gen = runST $ do
  g <- newSTRef gen
  let n = DV.length xs
      randomRST lohi = do
        (a, s') <- liftM (R.randomR lohi) (readSTRef g)
        writeSTRef g s'
        return a
      indexVector = DV.thaw $ DV.generate n id

  v <- indexVector
  forM_ [0 .. (n - 1)] $ \i -> do
    j <- randomRST (i, n - 1)
    GM.swap v i j
  ret <- DV.freeze v
  g' <- readSTRef g
  return (DV.backpermute xs ret, g')


shuffleListIO :: [a] -> IO (DV.Vector a)
shuffleListIO = shuffleVectorIO . DV.fromList

shuffleVectorIO :: DV.Vector a -> IO (DV.Vector a)
shuffleVectorIO !xs =  R.getStdRandom (shuffleVectorST xs)

shuffleVectorIO' :: DV.Vector a -> IO (DV.Vector a)
shuffleVectorIO' xs = R.getStdRandom (shuffleVector xs)

randomSplitIO :: Int -> DV.Vector a -> IO (DV.Vector a, DV.Vector a)
randomSplitIO n xs = liftM (DV.splitAt n) (shuffleVectorIO xs)


