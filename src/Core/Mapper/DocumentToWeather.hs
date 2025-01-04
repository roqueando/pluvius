module Core.Mapper.DocumentToWeather where

import Data.Maybe (mapMaybe)
import Core.Entity.Weather
import qualified Database.MongoDB as M

documentToWeather :: M.Document -> Maybe WeatherT
documentToWeather doc = do
  date <- M.lookup "date" doc
  hour <- M.lookup "hour" doc
  rain <- M.lookup "rain" doc
  pmax <- M.lookup "pmax" doc
  pmin <- M.lookup "pmin" doc
  tmax <- M.lookup "tmax" doc
  tmin <- M.lookup "tmin" doc
  dpmax <- M.lookup "dpmax" doc
  dpmin <- M.lookup "dpmin" doc
  hmax <- M.lookup "hmax" doc
  hmin <- M.lookup "hmin" doc
  pdiff <- M.lookup "pdiff" doc
  tdiff <- M.lookup "tdiff" doc
  dpdiff <- M.lookup "dpdiff" doc
  hdiff <- M.lookup "hdiff" doc
  pmaxAvg <- M.lookup "pmax_avg" doc
  pminAvg <- M.lookup "pmin_avg" doc
  tmaxAvg <- M.lookup "pmax_avg" doc
  tminAvg <- M.lookup "pmin_avg" doc
  dpmaxAvg <- M.lookup "pmax_avg" doc
  dpminAvg <- M.lookup "pmin_avg" doc
  hmaxAvg <- M.lookup "pmax_avg" doc
  hminAvg <- M.lookup "pmin_avg" doc

  return $ Weather
    { wDate = date
    , wHour = hour
    , wRain = rain
    , wPmax = pmax
    , wPmin = pmin
    , wTmax = tmax
    , wTmin = tmin
    , wDpmax = dpmax
    , wDpmin = dpmin
    , wHmax = hmax
    , wHmin = hmin
    , wPdiff = pdiff
    , wTdiff = tdiff
    , wDpdiff = dpdiff
    , wHdiff = hdiff
    , wPmaxAvg = pmaxAvg
    , wPminAvg = pminAvg
    , wTmaxAvg = tmaxAvg
    , wTminAvg = tminAvg
    , wDpmaxAvg = dpmaxAvg
    , wDpminAvg = dpminAvg
    , wHmaxAvg = hmaxAvg
    , wHminAvg = hminAvg
    }
docsToWeathers :: [M.Document] -> [WeatherT]
docsToWeathers = mapMaybe documentToWeather
