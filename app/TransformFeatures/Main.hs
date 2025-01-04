module Main where

import Core.UseCase.PipelineUseCase ( runTransform )
import External.Pipeline.Mongo ( getMongoCredentials )

main :: IO ()
main = do
  mongoConfig <- getMongoCredentials
  result <- runTransform mongoConfig "2019/01/01"
  case result of
    Left _ -> print ("Something goes wrong when running transformation" :: String)
    Right _ -> print ("Transformation ran successfully!" :: String)

