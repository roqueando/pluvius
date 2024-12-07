module File.CSV where

import Control.Monad.IO.Class
import Data.ByteString as BS
import qualified Data.ByteString as BL
import Data.Conduit (ConduitT, await, runConduitRes, (.|))
import qualified Data.Conduit.Combinators as CC
import Data.Csv
import Data.Text
import qualified Data.Vector as V

-- TODO: change this to Weather.Types
data Weather = Weather
  { date :: Text,
    hour :: Text,
    rain :: Float,
    pmax :: Float,
    pmin :: Float,
    tmax :: Float,
    tmin :: Float,
    dpmax :: Float,
    dpmin :: Float
    -- hmax :: Maybe Float
    -- hmin :: Float
  }
  deriving (Show)

instance FromNamedRecord Weather where
  parseNamedRecord r =
    Weather
      <$> r .: "date"
      <*> r .: "hour"
      <*> r .: "rain"
      <*> r .: "pmax"
      <*> r .: "pmin"
      <*> r .: "tmax"
      <*> r .: "tmin"
      <*> r .: "dpmax"
      <*> r .: "dpmin"

-- <*> r .: "hmax"
-- <*> r .: "hmin"

decodeCSV :: (MonadIO m) => ConduitT BS.ByteString Weather m ()
decodeCSV = do
  hdr <- await
  case hdr of
    Nothing -> return ()
    Just h -> do
      let decoded = decodeByName (BL.fromStrict h)
      case decoded of
        Left err -> do
          liftIO $ putStrLn err
        Right (hdd, v) -> do
          liftIO $ print hdd
          CC.yieldMany $ V.toList v

run :: IO ()
run = do
  runConduitRes $
    CC.sourceFile "./data/raw/2019.csv"
      .| decodeCSV
      -- .| CC.filter (\w -> rain w >= 0.0)
      .| CC.mapM_ (liftIO . print)
