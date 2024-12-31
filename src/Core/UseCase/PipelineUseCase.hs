module Core.UseCase.PipelineUseCase where

import Core.Adapter.Pipeline (PipelineError (..), Result (..), Pipeline, enrichData)

runPipeline :: (Pipeline a) => a -> String -> IO (Either PipelineError Result)
runPipeline = enrichData
