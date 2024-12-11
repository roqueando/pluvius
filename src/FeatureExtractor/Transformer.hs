module FeatureExtractor.Transformer (transformDate) where

import FeatureExtractor.Types (Weather(..))
import qualified Data.Text as T

type OneHotDate = Maybe (Int, Int, Int)

transformDate :: Weather -> OneHotDate
transformDate w = toTuple $ map read (splitted w)
  where
    splitted w' = map T.unpack (T.splitOn "/" $ date w')
    
    toTuple :: [a] -> Maybe (a, a, a)
    toTuple [x, y, z] = Just (x, y, z)
    toTuple _ = Nothing

transformHour :: Weather -> OneHotHour
transformHour w = toTuple $ map read (splitted w)
  where
    splitted w' = map T.unpack (T.splitOn " " $ hour w')

    -- "1200 UTC" -> ["1200", "UTC"] -> (splitAt 2)
    -- ["12", "00"] -> [x, y] -> x hour y minute
    toTuple :: [a] -> Maybe (a, a, a)
    toTuple [x, _] = Just x
    toTuple _ = Nothing
