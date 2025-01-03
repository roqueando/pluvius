module Main where

import Core.UseCase.PipelineUseCase ( runPipeline )
import External.Pipeline.Mongo ( getMongoCredentials )

main :: IO ()
main = do
  mongoConfig <- getMongoCredentials
  result <- runPipeline mongoConfig "2019/01/01"
  case result of
    Left _ -> print ("Something goes wrong when running pipeline" :: String)
    Right _ -> print ("Pipeline ran successfully!" :: String)

