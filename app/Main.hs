module Main where

import FeatureExtractor.Query (run)

main :: IO ()
main = do
  result <- run
  if length result > 0
     then print ("Something goes wrong" :: String)
     else print ("Features calculated successfully" :: String)
