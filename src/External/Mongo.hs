module External.Mongo where

import Core.DataPipeline
import Database.MongoDB
import qualified RIO.Text as T

data MongoT = Mongo
  { host' :: String,
    dbName :: Database,
    username :: Username,
    password :: Password
  }

connectAuthenticated :: MongoT -> IO (Either QueryError Pipe)
connectAuthenticated (Mongo h db username' password') = result
  where
    getPipe :: Pipe -> Pipe
    getPipe = connect (host h) >>= id

-- pipe <- connect (host h)
-- e <- access pipe master db (auth username' password')
-- if e
--   then return pipe
--   else error "Failed to authenticate"

runAggregate :: T.Text -> Action IO [Document]
runAggregate date = aggregate "raw" (pipeline date)
  where
    pipeline date' =
      [ ["$match" =: ["date" =: String date']],
        [ "$project"
            =: [ "date" =: Int64 1,
                 "hour" =: Int64 1,
                 "rain" =: Int64 1,
                 "pmax" =: ["$divide" =: [String "$pmax", Float 10.0]],
                 "pmin" =: ["$divide" =: [String "$pmin", Float 10.0]],
                 "tmax" =: Int64 1,
                 "tmin" =: Int64 1,
                 "dpmax" =: Int64,
                 "dpmin" =: Int64,
                 "hmax" =: Int64,
                 "hmin" =: Int64,
                 "pdiff" =: ["$subtract" =: [String "$pmax", String "$pmin"]],
                 "tdiff" =: ["$subtract" =: [String "$tmax", String "$tmin"]],
                 "dpdiff" =: ["$subtract" =: [String "$dpmax", String "$dpmin"]],
                 "hdiff" =: ["$subtract" =: [String "$hmax", String "$hmin"]],
                 "pmax_avg" =: ["$avg" =: String "$pmax"],
                 "pmin_avg" =: ["$avg" =: String "$pmin"],
                 "tmax_avg" =: ["$avg" =: String "$tmax"],
                 "tmin_avg" =: ["$avg" =: String "$tmin"],
                 "dpmax_avg" =: ["$avg" =: String "$dpmax"],
                 "dpmin_avg" =: ["$avg" =: String "$dpmin"],
                 "hmax_avg" =: ["$avg" =: String "$hmax"],
                 "hmin_avg" =: ["$avg" =: String "$hmin"]
               ]
        ],
        [ "$out"
            =: [ "db" =: String "feature_store",
                 "coll" =: String "enriched"
               ]
        ]
      ]

instance DataPipeline MongoT where
  enrichData mongo date = do
    pipe <- connectAuthenticated mongo
    result <- access pipe master "feature_store" (runAggregate date)
    case result of
      [] -> Right Success
      _ -> Left QueryError

-- instance DataPipeline (Action IO) where
--  enrichData = aggregate "raw" pipeline
--    where
--      pipeline = [ ["$project" =: ["$date" =: (1 :: Int)]]
--                 , ["$out" =: ["db" =: String "feature_store", "coll" =: String "enriched_tst"]]
--                 ]
