module Core.UseCase.PipelineUseCase where

import Core.Entity.Weather (EnrichedWeatherT (..))
import Core.Gateway.PipelineGateway
  ( PipelineError (..),
    PipelineGateway (..),
    calculateFeatures
  )

runPipeline :: (PipelineGateway a) => a -> String -> IO (Either PipelineError ())
runPipeline = calculateFeatures

runTransformation :: (PipelineGateway a) => a -> String -> IO (Either PipelineError [EnrichedWeatherT])
runTransformation = transformData
