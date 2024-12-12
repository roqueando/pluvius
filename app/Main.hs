module Main where

-- import FeatureExtractor.ExtractCSV (run)
import Frames 
import Frames.TH 
import Pipes (Producer)
import Lens.Micro
import Lens.Micro.Extras
import qualified Control.Foldl as L

type Item =
  Record ["id" :-> Int
         , "date" :-> Text
         , "hour" :-> Text
         , "pmax" :-> Maybe Float
         , "pmin" :-> Maybe Float
         , "tmax" :-> Maybe Float
         , "tmin" :-> Maybe Float
         , "dpmax" :-> Maybe Float
         , "dpmin" :-> Maybe Float
         , "hmax" :-> Maybe Float
         , "hmin" :-> Maybe Float
      ]

tableTypes "Item" "./data/raw/2019_test.csv"


itemStream :: MonadSafe m => Producer Item m ()
itemStream = readTableOpt itemParser "./data/raw/2019_test.csv"

loadItems :: IO (Frame Item)
loadItems = inCoreAoS itemStream

minMax :: Ord a => L.Fold a (Maybe a, Maybe a)
minMax = (,) <$> L.maximum <*> L.minimum

main :: IO ()
main = putStrLn "hello there"
