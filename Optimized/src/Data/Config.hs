module Data.Config where

data Config = Config { dampingTerm :: Double
                     , numDimensions :: Int
                     , numIterations :: Int
                     , startingEstimate :: Double
                     , learningRate :: Double
                     , regFactor :: Double
                     }

