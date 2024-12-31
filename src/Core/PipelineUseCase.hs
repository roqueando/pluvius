module Core.PipelineUseCase where

import Core.DataPipeline (PipelineError (..), Result (..), DataPipeline, enrichData)

runPipeline :: (DataPipeline a) => a -> String -> IO (Either PipelineError Result)
runPipeline = enrichData
