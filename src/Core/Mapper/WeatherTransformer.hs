module Core.Mapper.WeatherTransformer where

import Core.Entity.Weather (WeatherT (..), TransformedWeatherT(..))
import qualified RIO.Text as T

data Date =
  Day
    | Month
    | Year
    deriving (Eq)

toTransformedWeather :: WeatherT -> TransformedWeatherT
toTransformedWeather _ =
  TransformedWeather
    { twDay = 1
    , twMonth = 1
    , twYear = 1
    , twHour = 1
    , twMinute = 1
    , twPmax = 1.0
    , twPmin = 1.0
    , twTmax = 1.0
    , twTmin = 1.0
    , twDpmax = 1.0
    , twDpmin = 1.0
    , twHmax = 1.0
    , twHmin = 1.0
    , twPdiff = 1.0
    , twTdiff = 1.0
    , twDpdiff = 1.0
    , twHdiff = 1.0
    , twPmaxAvg = 1.0
    , twPminAvg = 1.0
    , twTmaxAvg = 1.0
    , twTminAvg = 1.0
    , twDpmaxAvg = 1.0
    , twDpminAvg = 1.0
    , twHmaxAvg = 1.0
    , twHminAvg = 1.0
    }

getOneHotDate :: T.Text -> Date -> T.Text
getOneHotDate dateStr dateField
  | dateField == Day = T.split (=='/') dateStr !! 2
  | dateField == Month = T.split (=='/') dateStr !! 1
  | dateField == Year = head (T.split (=='/') dateStr)
  | otherwise = "0"
