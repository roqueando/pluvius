module Main where

-- import FeatureExtractor.Query (run)
import Config ( ConfigT(..) )
import Core.DataPipeline ( Date )
import Core.PipelineUseCase ( runPipeline )
import External.Mongo ( MongoT(..) )

-- TODO: get these variables by env vars
mongoConfig :: MongoT
mongoConfig =
  Mongo
    { host' = "127.0.0.1",
      dbName = "feature_store",
      username = "pluvius",
      password = "local_password"
    }

-- | High Order Function that takes a ConfigT, a Date (string) and return a String
runWithConfig :: ConfigT -> Date -> String
runWithConfig (ConfigT {database = db}) = runPipeline db

main :: IO ()
main = do
  let config = ConfigT {database = mongoConfig}
  let result = runWithConfig config "2019/01/01"
  print result

-- result <- run
-- if length result > 0
--   then print ("Something goes wrong" :: String)
--   else print ("Features calculated successfully" :: String)
