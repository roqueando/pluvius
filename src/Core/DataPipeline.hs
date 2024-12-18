module Core.DataPipeline where

class Monad m => DataPipeline m where
  enrichData :: String -> m ()
