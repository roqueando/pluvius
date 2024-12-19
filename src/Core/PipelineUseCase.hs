module Core.PipelineUseCase where

import Core.DataPipeline

runPipeline :: (DataPipeline a) => a -> String -> String
runPipeline = enrichData
