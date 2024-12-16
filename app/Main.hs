module Main where

-- import FeatureExtractor.ExtractCSV (run)
import FeatureExtractor.Query (run)

main :: IO ()
main = do
  result <- run
  print result
