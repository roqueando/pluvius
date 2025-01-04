module Core.UseCase.PipelineUseCase where

import Core.Entity.Weather (WeatherT (..), TransformedWeatherT(..))
import Core.Mapper.WeatherTransformer
import Core.Gateway.PipelineGateway
  ( PipelineError (..),
    PipelineGateway (..),
    calculateFeatures
  )

runPipeline :: (PipelineGateway a) => a -> String -> IO (Either PipelineError ())
runPipeline = calculateFeatures

transformData :: (PipelineGateway a) => a -> String -> IO (Either PipelineError [TransformedWeatherT])
transformData gateway date = do
  fetchedData <- fetchData gateway date 
  case fetchedData of
    Left qe -> return $ Left qe
    Right xs ->  return $ Right $ transformWeather xs

transformWeather :: [WeatherT] -> [TransformedWeatherT]
transformWeather = map toTransformedWeather

saveTransformedData :: (PipelineGateway a) => a -> String -> IO (Either PipelineError ())
saveTransformedData = insertTransformedData
