module FeatureExtractor.Query where

import Database.MongoDB

-- import Control.Monad.Trans (liftIO)

data QueryError
  = NotFound
  | MemoryError

type Result = [Document]


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
                       , "pmax" =: ["$divide" =: [String "$pmax", Float 10.0]]
                       , "pmin" =: ["$divide" =: [String "$pmin", Float 10.0]]
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
                       , "pmax_avg" =: ["$avg" =: ("$pmax" :: String)]
                       , "pmin_avg" =: ["$avg" =: ("$pmin" :: String)]
                       , "tmax_avg" =: ["$avg" =: ("$tmax" :: String)]
                       , "tmin_avg" =: ["$avg" =: ("$tmin" :: String)]
                       , "dpmax_avg" =: ["$avg" =: ("$dpmax" :: String)]
                       , "dpmin_avg" =: ["$avg" =: ("$dpmin" :: String)]
                       , "hmax_avg" =: ["$avg" =: ("$hmax" :: String)]
                       , "hmin_avg" =: ["$avg" =: ("$hmin" :: String)]
                       ]]
        , ["$out" =: ["db" =: ("feature_store" :: String)
                     , "coll" =: ("enriched" :: String)
                     ]]
        ]

run :: IO [Document]
run = do
  pipe <- connectAuthenticated "127.0.0.1" "admin" "pluvius" "local_password"
  access pipe master "feature_store" enrichData
