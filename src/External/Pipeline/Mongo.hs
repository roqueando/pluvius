{-# LANGUAGE InstanceSigs #-}
module External.Pipeline.Mongo where

import Database.MongoDB
import System.Environment ( getEnv )

import Core.Gateway.PipelineGateway (PipelineGateway (calculateFeatures))
import Core.Entity.Weather (EnrichedWeatherT (..), WeatherT (..))
import qualified Core.Gateway.PipelineGateway as P

import qualified RIO.Text as T

data MongoT = Mongo
  { host' :: String,
    dbName :: Database,
    username :: Username,
    password :: Password
  }

instance P.PipelineGateway MongoT where
  calculateFeatures :: MongoT -> String -> IO (Either P.PipelineError ())
  calculateFeatures mongo date = do
    pipe <- connectAuthenticated mongo
    case pipe of
      Left qe -> return $ Left qe
      Right p -> do
        result <- access p master "feature_store" (runAggregate (T.pack date))
        handleResult result

  fetchData :: MongoT -> String -> IO (Either P.PipelineError [WeatherT])
  fetchData _ _ = undefined

  transformData :: MongoT -> String -> IO (Either P.PipelineError [EnrichedWeatherT])
  transformData mongo date = do
    pipe <- connectAuthenticated mongo
    case pipe of
      Left qe -> return $ Left qe
      Right p -> do
        result <- access p master "feature_store" (runTransform (T.pack date))
        handleMyResult result
    where
      handleMyResult [] = return $ Left P.CommonError
      handleMyResult _ = return $ Right []

handleResult :: [Document] -> IO (Either P.PipelineError ())
handleResult [] = return $ Right ()
handleResult _ = return $ Left P.CommonError

getMongoCredentials :: IO MongoT
getMongoCredentials = do
  host'' <- getEnv "DB_HOST"
  dbName' <- getEnv "DB_NAME"
  dbUser <- getEnv "DB_USER"
  dbPassword <- getEnv "DB_PASSWORD"
  return Mongo
    { host' = host'',
      dbName = T.pack dbName',
      username = T.pack dbUser,
      password = T.pack dbPassword
    }


connectAuthenticated :: MongoT -> IO (Either P.PipelineError Pipe)
connectAuthenticated (Mongo h db username' password') = do
  pipe <- connect (host h)
  e <- access pipe master db (auth username' password')
  if e
    then return $ Right pipe
    else error "Failed to authenticate"

runTransform :: T.Text -> Action IO [Document]
runTransform _ = undefined

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
                 "dpmax" =: Int64 1,
                 "dpmin" =: Int64 1,
                 "hmax" =: Int64 1,
                 "hmin" =: Int64 1,
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
