module Core.Adapter.Pipeline where

data PipelineError
  = PipelineError
  | AuthenticationError
  | TimeoutError
  | CommonError

data Result =
  Success

class Pipeline a where
  enrichData :: a -> String -> IO (Either PipelineError Result)
