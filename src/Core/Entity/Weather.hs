module Core.Entity.Weather where

import GHC.Generics (Generic)

data WeatherT = Weather
  { wDate :: String
  , wHour :: String
  , wRain :: Float
  , wPmax :: Float
  , wPmin :: Float
  , wTmax :: Float
  , wTmin :: Float
  , wDpmax :: Float
  , wDpmin :: Float
  , wHmax :: Float
  , wHmin :: Float
  , wPdiff :: Float
  , wTdiff :: Float
  , wDpdiff :: Float
  , wHdiff :: Float
  , wPmaxAvg :: Float
  , wPminAvg :: Float
  , wTmaxAvg :: Float
  , wTminAvg :: Float
  , wDpmaxAvg :: Float
  , wDpminAvg :: Float
  , wHmaxAvg :: Float
  , wHminAvg :: Float
  } deriving (Generic)

data TransformedWeatherT = TransformedWeather
  { twDay :: Int
  , twMonth :: Int
  , twYear :: Int
  , twHour :: Int
  , twMinute :: Int
  , twPmax :: Float
  , twPmin :: Float
  , twTmax :: Float
  , twTmin :: Float
  , twDpmax :: Float
  , twDpmin :: Float
  , twHmax :: Float
  , twHmin :: Float
  , twPdiff :: Float
  , twTdiff :: Float
  , twDpdiff :: Float
  , twHdiff :: Float
  , twPmaxAvg :: Float
  , twPminAvg :: Float
  , twTmaxAvg :: Float
  , twTminAvg :: Float
  , twDpmaxAvg :: Float
  , twDpminAvg :: Float
  , twHmaxAvg :: Float
  , twHminAvg :: Float
  }
