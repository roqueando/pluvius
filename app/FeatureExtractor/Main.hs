module Main where

import Core.Entity.Settings ( SettingsT(..) )
import Core.Adapter.Pipeline
import Core.UseCase.PipelineUseCase ( runPipeline )
import External.Pipeline.Mongo ( MongoT(..) )

-- TODO: get these variables by env vars
mongoConfig :: MongoT
mongoConfig =
  Mongo
    { host' = "127.0.0.1",
      dbName = "admin",
      username = "pluvius",
      password = "local_password"
    }

-- | High Order Function that takes a SettingsT, a Date (string) and return a String
runWithConfig :: SettingsT -> String -> IO (Either PipelineError Result)
runWithConfig (SettingsT {database = db}) = runPipeline db

main :: IO ()
main = do
  let config = SettingsT {database = mongoConfig}
  result <- runWithConfig config "2019/01/01"
  case result of
    Left _ -> print ("Something goes wrong when running pipeline" :: String)
    Right _ -> print ("Pipeline ran successfully!" :: String)
