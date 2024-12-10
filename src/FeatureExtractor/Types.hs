module FeatureExtractor.Types (Weather (..), EnrichedWeather(..)) where

import qualified Data.Text as T
import Data.Csv ((.:), FromNamedRecord(..))

data Weather = Weather
  { date :: T.Text,
    hour :: T.Text,
    rain :: Maybe Float,
    pmax :: Maybe Float,
    pmin :: Maybe Float,
    tmax :: Maybe Float,
    tmin :: Maybe Float,
    dpmax :: Maybe Float,
    dpmin :: Maybe Float,
    hmax :: Maybe Float,
    hmin :: Maybe Float
  }
  deriving (Show)

data EnrichedWeather = EnrichedWeather
  { weather :: Weather
  , day :: Int
  , month :: Int
  , year :: Int
  , eHour :: Int
  , minute :: Int

  -- Min and Max by day of month (DoM)
  , pMaxDom :: Float -- Min and Max by day of month (DoM)
  , pMinDom :: Float
  , tMaxDom :: Float
  , tMinDom :: Float
  , dpMaxDom :: Float
  , dpMinDom :: Float
  , hMaxDom :: Float
  , hMinDom :: Float

  -- Difference from Min Max by day of month
  , pDiff :: Float
  , tDiff :: Float
  , dpDiff :: Float
  , hDiff :: Float

  -- Average (mean) from min max by day of month
  , pAvgMinDom :: Float
  , pAvgMaxDom :: Float
  , tAvgMinDom :: Float
  , tAvgMaxDom :: Float
  , dpAvgMinDom :: Float
  , dpAvgMaxDom :: Float
  , hAvgMinDom :: Float
  , hAvgMaxDom :: Float
  }

instance FromNamedRecord Weather where
  parseNamedRecord r =
    Weather
      <$> r .: "date"
      <*> r .: "hour"
      <*> r .: "rain"
      <*> r .: "pmax"
      <*> r .: "pmin"
      <*> r .: "tmax"
      <*> r .: "tmin"
      <*> r .: "dpmax"
      <*> r .: "dpmin"
      <*> r .: "hmax"
      <*> r .: "hmin"
