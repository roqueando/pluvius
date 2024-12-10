module FeatureExtractor.Types (Weather (..)) where

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
  { day :: Int
  , month :: Int
  , year :: Int
  , eHour :: Int
  , minute :: Int
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
