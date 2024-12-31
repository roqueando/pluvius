module Main where

import Config ( ConfigT(..) )
import Core.DataPipeline
import Core.PipelineUseCase ( runPipeline )
import External.Mongo ( MongoT(..) )

-- TODO: get these variables by env vars
mongoConfig :: MongoT
mongoConfig =
  Mongo
    { host' = "127.0.0.1",
      dbName = "admin",
      username = "pluvius",
      password = "local_password"
    }

-- | High Order Function that takes a ConfigT, a Date (string) and return a String
runWithConfig :: ConfigT -> String -> IO (Either PipelineError Result)
runWithConfig (ConfigT {database = db}) = runPipeline db

main :: IO ()
main = do
  let config = ConfigT {database = mongoConfig}
  result <- runWithConfig config "2019/01/01"
  case result of
    Left _ -> print ("Something goes wrong when running pipeline" :: String)
    Right _ -> print ("Pipeline ran successfully!" :: String)

