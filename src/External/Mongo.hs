{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module External.Mongo where

import Core.DataPipeline
import Database.MongoDB

instance DataPipeline (Action IO) where
  enrichData = aggregate "raw" pipeline
    where
      pipeline = [ ["$project" =: ["$date" =: (1 :: Int)]]
                 , ["$out" =: ["db" =: String "feature_store", "coll" =: String "enriched_tst"]]
                 ]
