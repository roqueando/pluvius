module FeatureExtractor.Query where

import Database.MongoDB

-- import Control.Monad.Trans (liftIO)

data QueryError
  = NotFound
  | MemoryError

type Result = [Document]

executeQuery :: Action IO [Document]
executeQuery = getRawData

getRawData :: Action IO [Document]
getRawData = find (select ["date" =: ("2019/01/01" :: String)] "raw") >>= rest

run :: IO [Document]
run = do
  pipe <- connect (host "127.0.0.1") -- change to a env variable
  result <- access pipe master "feature_store" (auth "pluvius" "local_password")
  case result of
    True -> do 
      result <- access pipe master "feature_store" executeQuery
      close pipe
      return result
    False -> do
      print "hello to an error"
      close pipe
      return []

