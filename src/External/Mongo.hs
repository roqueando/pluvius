{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module External.Mongo where

import Database.MongoDB
import Core.DataPipeline

data MongoT = Mongo
  { host' :: String
  , dbName :: Database
  , username :: Username
  , password :: Password
  }

connectAuthenticated :: MongoT -> IO Pipe
connectAuthenticated (Mongo h db username' password') = do
  pipe <- connect (host h)
  e <- access pipe master db (auth username' password')
  if e
     then return pipe
     else error "Failed to authenticate"

instance DataPipeline MongoT where
  enrichData (Mongo h _ _ _) date = h ++ date
--instance DataPipeline (Action IO) where
--  enrichData = aggregate "raw" pipeline
--    where
--      pipeline = [ ["$project" =: ["$date" =: (1 :: Int)]]
--                 , ["$out" =: ["db" =: String "feature_store", "coll" =: String "enriched_tst"]]
--                 ]
