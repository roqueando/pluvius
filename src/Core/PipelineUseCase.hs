module Core.PipelineUseCase where

import Core.DataPipeline

runPipeline :: (DataPipeline m) => String -> m ()
runPipeline date = enrichData date
