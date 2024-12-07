module File.CSV where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Conduit (ConduitT, await, runConduitRes, yield, (.|))
import qualified Data.Conduit.Combinators as CC
import Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

-- TODO: change this to Weather.Types
data Weather = Weather
  { date :: T.Text,
    hour :: T.Text,
    rain :: Maybe Float,
    pmax :: Maybe Float,
    pmin :: Maybe Float,
    tmax :: Maybe Float,
    tmin :: Maybe Float,
    dpmax :: Maybe Float,
    dpmin :: Maybe Float,
    hmax :: Maybe Float,
    hmin :: Maybe Float
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
      <*> r .: "hmax"
      <*> r .: "hmin"

decodeCSV :: (MonadIO m) => ConduitT BS.ByteString Weather m ()
decodeCSV = do
  csvData <- CC.sinkLazy
  let decoded = decodeByName csvData
  case decoded of
    Left err -> do
      liftIO $ putStrLn err
    Right (_, v) -> do
      CC.yieldMany $ V.toList v

limitPrint :: (MonadIO m) => ConduitT Weather Void m ()
limitPrint = CC.mapM_ $ \weather -> do
  liftIO $ print weather
  liftIO $ hFlush stdout

run :: IO ()
run = do
  runConduitRes $
    CC.sourceFile "./data/raw/2019.csv"
      .| decodeCSV
      .| CC.filter precipitation
      .| CC.sinkNull
  where
    precipitation (Weather {rain = Nothing}) = False
    precipitation (Weather {rain = Just rx}) = rx >= 0.0
