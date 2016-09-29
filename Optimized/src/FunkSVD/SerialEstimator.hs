{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module FunkSVD.SerialEstimator ( FunkSVD()
                               , runFullEstimationIO
                               , runFullEstimationOnListIO
                               ) where

import qualified Data.Vector as DV
import qualified Data.Map.Strict as M

import qualified Randomizer.RandomUtils as R

import qualified Control.Monad.State.Strict as S

import Data.Maybe (fromMaybe)

import System.Random (getStdRandom, RandomGen)

import Control.Monad.Reader

import Data.Rating
import Data.Config

type User = Int
type Item = Int

type FunkSVDReader = Reader Config

data FunkSVD = FunkSVD { residuals :: DV.Vector Rating
                       , meanRating :: Double
                       , userOffsets :: M.Map User Double
                       , itemOffsets :: M.Map Item Double
                       , userVectors :: M.Map User (DV.Vector Double)
                       , itemVectors :: M.Map Item (DV.Vector Double)
                       }

instance Show FunkSVD where
  show f = "{meanRating = " ++ (show $ meanRating f) ++ ", Residuals RMSE = " ++ show (sqrt (globalMean (**2) (residuals f))) ++ "}"


globalMean :: Fractional a => (Double -> a) -> DV.Vector Rating -> a
globalMean func ratings = totalRating / fromIntegral (DV.length ratings)
  where totalRating = DV.sum $ fmap (func . rating) ratings

groupByKey :: Ord k => DV.Vector (k, a) -> M.Map k (DV.Vector a)
groupByKey = DV.foldl' (\m (k, v) -> addKVToMap m k v) M.empty
  where addKVToMap m k v = M.insertWith (DV.++) k (DV.singleton v) m

genUserOffsetMap :: DV.Vector Rating -> Double -> FunkSVDReader (M.Map User Double)
genUserOffsetMap ratings mean = do
  damp <- asks dampingTerm
  let
    userMap           = groupByKey (fmap (\(Rating r u _) -> (u, r)) ratings)
    totalRatings vs   = DV.sum vs - (fromIntegral (DV.length vs) * mean)
    denominator vs    = fromIntegral (DV.length vs) + damp
    userOffsetFunc vs = totalRatings vs / denominator vs
  return $ fmap userOffsetFunc userMap

genItemOffsetMap :: M.Map User Double -> DV.Vector Rating -> Double -> FunkSVDReader (M.Map Item Double)
genItemOffsetMap userMap ratings mean = do
  damp <- asks dampingTerm
  let
    userOffset u                  = fromMaybe 0.0 $ M.lookup u userMap
    ratingsAfterUserOffsetAndMean = fmap (\r -> (item r, rating r - mean - userOffset (user r))) ratings
    groupedRatings                = groupByKey ratingsAfterUserOffsetAndMean
  return $ fmap (\rs -> DV.sum rs / (fromIntegral (DV.length rs) + damp)) groupedRatings

normalizeRatings :: Functor f => f Rating -> M.Map User Double -> M.Map Item Double -> Double -> f Rating
normalizeRatings ratings userMap itemMap mean = fmap offsetRating ratings
  where
    offsetRating (Rating r u i) =
      let (Just ui) = M.lookup u userMap
          (Just ii) = M.lookup i itemMap
       in Rating (r - mean - ui - ii) u i

generateFunkSVDInput :: DV.Vector Rating -> FunkSVDReader FunkSVD
generateFunkSVDInput ratings = do
  let mean = globalMean id ratings
  userOffs <- genUserOffsetMap ratings mean
  itemOffs <- genItemOffsetMap userOffs ratings mean
  let normRatings = normalizeRatings ratings userOffs itemOffs mean
      userV       = DV.foldl' (\m r -> M.insert (user r) DV.empty m) M.empty ratings
      itemV       = DV.foldl' (\m r -> M.insert (item r) DV.empty m) M.empty ratings
  return $ FunkSVD normRatings mean userOffs itemOffs userV itemV

runIteration :: Config -> (M.Map User Double, M.Map Item Double) -> DV.Vector Rating -> (M.Map User Double, M.Map Item Double)
runIteration config maps res = DV.foldl' f maps res
  where
    f (uV, iV) (Rating r u i) = uV' `seq` iV' `seq` (uV', iV')
      where
        dStart = startingEstimate config
        lRate = learningRate config
        reg = regFactor config
        ua  = M.findWithDefault dStart u uV
        ia  = M.findWithDefault dStart i iV
        eps = (r - ua * ia)
        dua = lRate * (eps * ia - reg * ua)
        dia = lRate * (eps * ua - reg * ia)
        ua' = ua + dua
        ia' = ia + dia
        uV' = ua' `seq` M.insert u ua' uV
        iV' = ia' `seq` M.insert i ia' iV

appendVectors :: Ord k => M.Map k (DV.Vector a) -> M.Map k a -> M.Map k (DV.Vector a)
appendVectors v dv = M.mapWithKey f v
  where f k vs = let (Just dv') = M.lookup k dv in DV.snoc vs dv'

updateResiduals :: Functor f => f Rating -> M.Map User Double -> M.Map Item Double -> f Rating
updateResiduals res dUv dIv = fmap f res
  where
    f (Rating r u i) = Rating (r - ua * ia) u i
      where
        (Just ua) = M.lookup u dUv
        (Just ia) = M.lookup i dIv

runTillConvergence :: RandomGen t => Config -> FunkSVD -> S.State t FunkSVD
runTillConvergence config funkSVD =
  do

  let func (!dUv, !dIv) _ = do
        shuffledResiduals <- R.shuffleVectorState $ residuals funkSVD
        return $ runIteration config (dUv, dIv) shuffledResiduals

  (dUv, dIv) <- foldM func (M.empty, M.empty) [1 .. (numIterations config)]

  let residuals' = updateResiduals (residuals funkSVD) dUv dIv
      uV = userVectors funkSVD
      iV = itemVectors funkSVD
  return funkSVD{residuals = residuals', userVectors = appendVectors uV dUv, itemVectors = appendVectors iV dIv}

runFullEstimationIO :: DV.Vector Rating -> Config -> IO FunkSVD
runFullEstimationIO ratings config =
  do
  (trainingSet, testSet) <- R.randomSplitIO (4 * DV.length ratings `div` 5) ratings
  let funkSVDInput = runReader (generateFunkSVDInput trainingSet) config

  let func !funkSVD dim = do
        putStrLn $ "Working on dimension " ++ show dim
        funkSVD' <- getStdRandom $ S.runState $ runTillConvergence config funkSVD
        putStrLn $ "Average residuals = " ++ (show . globalMean abs) (residuals funkSVD')
        putStrLn $ "Residuals RMSE = " ++ show (sqrt (globalMean (**2) (residuals funkSVD')))
        putStrLn $ "Training RMSE = " ++ show (getRMSE trainingSet funkSVD')
        putStrLn $ "Test RMSE = " ++ show (getRMSE testSet funkSVD')
        return funkSVD'

  foldM func funkSVDInput [1 .. (numDimensions config)]

runFullEstimationOnListIO :: [Rating] -> Config -> IO FunkSVD
runFullEstimationOnListIO ratings = runFullEstimationIO $ DV.fromList ratings

predict :: FunkSVD -> User -> Item -> Double
predict funkSVD u i =
  (meanRating funkSVD) + DV.sum (DV.zipWith func uV iV) + uOff + iOff
    where uOff = M.findWithDefault 0.0 u $ userOffsets funkSVD
          iOff = M.findWithDefault 0.0 i $ itemOffsets funkSVD
          uV = M.findWithDefault DV.empty u $ userVectors funkSVD
          iV = M.findWithDefault DV.empty i $ itemVectors funkSVD
          func x y = min (x * y) 5.0

getRMSE :: DV.Vector Rating -> FunkSVD -> Double
getRMSE testSet funkSVD = sqrt (errorSum / fromIntegral (DV.length actuals))
  where errorSum = DV.sum $ DV.zipWith (\x y -> (x - y) ** 2) actuals estimates
        predictFunc = predict funkSVD
        estimates = DV.map (\(Rating _ u i) -> predictFunc u i) testSet
        actuals = DV.map rating testSet



