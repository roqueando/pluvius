module Core.DataPipeline where

type Date = String

data QueryError
  = QueryError
  | AuthenticationError
  | TimeoutError

data Success = Success

class DataPipeline a where
  enrichData :: a -> Date -> Either QueryError b
