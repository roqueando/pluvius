module FeatureExtractor.Query where

import Database.MongoDB

-- import Control.Monad.Trans (liftIO)

data QueryError
  = NotFound
  | MemoryError

type Result = [Document]

executeQuery :: Action IO [Document]
executeQuery = getRawData


connectAuthenticated :: String -> Database -> Username -> Password -> IO Pipe
connectAuthenticated host' dbName username password = do
  pipe <- connect (host host')
  -- \$> :t pipe
  e <- access pipe master dbName (auth username password)
  if e
    then return pipe
    else error "Failed to authenticate"

enrichData :: Action IO [Document]
enrichData = aggregate "raw" pipeline
  where
    pipeline =
      [ ["$match" =: ["date" =: ("2019/01/01" :: String)]] -- TODO: change to today
      , ["$project" =: [ "date" =: (1 :: Int)
                       , "hour" =: (1 :: Int)
                       , "rain" =: (1 :: Int)
                       , "pmax" =: (1 :: Int)
                       , "pmin" =: (1 :: Int)
                       , "tmax" =: (1 :: Int)
                       , "tmin" =: (1 :: Int)
                       , "dpmax" =: (1 :: Int)
                       , "dpmin" =: (1 :: Int)
                       , "hmax" =: (1 :: Int)
                       , "hmin" =: (1 :: Int)
                       , "pdiff" =: ["$subtract" =: [("$pmax" :: String), ("$pmin" :: String)]]
                       , "tdiff" =: ["$subtract" =: [("$tmax" :: String), ("$tmin" :: String)]]
                       , "dpdiff" =: ["$subtract" =: [("$dpmax" :: String), ("$dpmin" :: String)]]
                       , "hdiff" =: ["$subtract" =: [("$hmax" :: String), ("$hmin" :: String)]]
                       ]]

      ]

getRawData :: Action IO [Document]
getRawData = find (select ["date" =: ("2019/01/01" :: String)] "raw") >>= rest

run :: IO [Document]
run = do
  pipe <- connectAuthenticated "127.0.0.1" "admin" "pluvius" "local_password"
  access pipe master "feature_store" executeQuery
