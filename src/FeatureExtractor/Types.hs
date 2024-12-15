module FeatureExtractor.Types (Weather (..), EnrichedWeather (..)) where

import Data.Csv
  ( FromNamedRecord (..),
    ToRecord (..),
    record,
    toField,
    (.:),
  )
import qualified Data.Text as T

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
  { day :: Int,
    month :: Int,
    year :: Int,
    eHour :: Int,
    minute :: Int,
    -- Min and Max by day of month (DoM)
    pMaxDom :: Float, -- Min and Max by day of month (DoM)
    pMinDom :: Float,
    tMaxDom :: Float,
    tMinDom :: Float,
    dpMaxDom :: Float,
    dpMinDom :: Float,
    hMaxDom :: Float,
    hMinDom :: Float,
    -- Difference from Min Max by day of month
    pDiff :: Float,
    tDiff :: Float,
    dpDiff :: Float,
    hDiff :: Float,
    -- Average (mean) from min max by day of month
    pAvgMinDom :: Float,
    pAvgMaxDom :: Float,
    tAvgMinDom :: Float,
    tAvgMaxDom :: Float,
    dpAvgMinDom :: Float,
    dpAvgMaxDom :: Float,
    hAvgMinDom :: Float,
    hAvgMaxDom :: Float
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

instance ToRecord EnrichedWeather where
  toRecord
    (EnrichedWeather day' month' year' eHour' minute' pMaxDom' pMinDom' tMaxDom' tMinDom' dpMaxDom' dpMinDom' hMaxDom' hMinDom' pDiff' tDiff' dpDiff' hDiff' pAvgMinDom' pAvgMaxDom' tAvgMinDom' tAvgMaxDom' dpAvgMinDom' dpAvgMaxDom' hAvgMinDom' hAvgMaxDom') =
      record
        [ toField day',
          toField month',
          toField year',
          toField eHour',
          toField minute',
          toField pMaxDom',
          toField pMinDom',
          toField tMaxDom',
          toField tMinDom',
          toField dpMaxDom',
          toField dpMinDom',
          toField hMaxDom',
          toField hMinDom',
          toField pDiff',
          toField tDiff',
          toField dpDiff',
          toField hDiff',
          toField pAvgMinDom',
          toField pAvgMaxDom',
          toField tAvgMinDom',
          toField tAvgMaxDom',
          toField dpAvgMinDom',
          toField dpAvgMaxDom',
          toField hAvgMinDom',
          toField hAvgMaxDom'
        ]
