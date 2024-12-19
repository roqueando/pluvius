module Core.DataPipeline where

class DataPipeline a where
  enrichData :: a -> String -> String
