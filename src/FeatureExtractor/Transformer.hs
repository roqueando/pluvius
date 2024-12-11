module FeatureExtractor.Transformer (transformDate, transformHour) where

import FeatureExtractor.Types (Weather(..))
import qualified Data.Text as T

type OneHotDate = Maybe (Int, Int, Int)
type OneHotHour = Maybe (Int, Int)

transformDate :: Weather -> OneHotDate
transformDate w = toTuple $ map read (splitted w)
  where
    splitted w' = map T.unpack (T.splitOn "/" $ date w')

    toTuple :: [a] -> Maybe (a, a, a)
    toTuple [x, y, z] = Just (x, y, z)
    toTuple _ = Nothing

transformHour :: Weather -> OneHotHour
transformHour w = toTuple $ getHourAndMinute $ splitByWhitespace w
  where
    splitByWhitespace = T.splitOn " " . hour
    getHourAndMinute spl = T.splitAt 2 (head spl)

    toTuple :: (T.Text, T.Text) -> Maybe (a, a)
    toTuple (x, y) = Just (read $ T.unpack x, read $ T.unpack y)
    toTuple _ = Nothing
