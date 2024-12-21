module Core.PipelineUseCase where

import Core.DataPipeline (QueryError (..), Success (..), DataPipeline, enrichData)

runPipeline :: (DataPipeline a) => a -> String -> Either QueryError Success
runPipeline = enrichData
