module Core.Entity.Weather where

data WeatherT = Weather
  { wDate :: String
  , wHour :: String
  }

data EnrichedWeatherT = EnrichedWeather
  { ewDate :: String
  , ewHour :: String
  }
