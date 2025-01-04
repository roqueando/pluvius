module Core.Gateway.PipelineGateway where

import Core.Entity.Weather (WeatherT (..))

data PipelineError
  = PipelineError
  | AuthenticationError
  | TimeoutError
  | CommonError
  | QueryResultError

class PipelineGateway a where
  calculateFeatures :: a -> String -> IO (Either PipelineError ())
  fetchData :: a -> String -> IO (Either PipelineError [WeatherT])
  insertTransformedData :: a -> String -> IO (Either PipelineError ())
