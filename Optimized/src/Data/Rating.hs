{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Rating where

import qualified Data.Text as T

data Rating = Rating { rating :: !Double
                     , user :: !Int
                     , item :: !Int
                     } deriving (Show)

parseRating :: T.Text -> Rating
parseRating rawRating =
  case T.splitOn "|" rawRating of
    [r, u, i] ->
      Rating (read $ T.unpack r) (read $ T.unpack u) (read $ T.unpack i)
    _ -> error $ "Unable to decode " ++ T.unpack rawRating ++ " <- this string"

