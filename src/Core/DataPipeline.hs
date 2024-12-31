module Core.DataPipeline where

data PipelineError
  = PipelineError
  | AuthenticationError
  | TimeoutError
  | CommonError

data Result =
  Success

class DataPipeline a where
  enrichData :: a -> String -> IO (Either PipelineError Result)
