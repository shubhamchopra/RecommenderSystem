{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment

import FunkSVD.SerialEstimator

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Codec.Compression.GZip (decompress)

import Data.Maybe (mapMaybe)

import Data.Rating
import Data.Config

readCompressedFile :: FilePath -> IO [T.Text]
readCompressedFile fileName = fmap (func . B.split '\n' . decompress) $ B.readFile fileName
  where func = mapMaybe (\r -> if B.null r then Nothing else Just $ (decodeUtf8 . B.toStrict) r)

readReviews :: FilePath -> IO [Rating]
readReviews ratingsFile = fmap (fmap parseRating) $ readCompressedFile ratingsFile

getConfig :: Config
getConfig = Config { dampingTerm = 25.0
                     , numDimensions = 5
                     , numIterations = 100
                     , startingEstimate = 0.1
                     , learningRate = 0.001
                     , regFactor = 0.02
                   }

main :: IO ()
main = do
  [ratingFile] <- getArgs
  ratings <- readReviews ratingFile
  putStrLn "Got ratings"
  funkSVD <- runFullEstimationOnListIO ratings getConfig
  print funkSVD

