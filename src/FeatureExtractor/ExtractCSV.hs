module FeatureExtractor.ExtractCSV where

import qualified Data.ByteString as BS
import qualified Data.Conduit.Combinators as CC

import Data.Conduit.List (chunksOf)
import Data.Conduit (ConduitT, runConduitRes, (.|), await, yield)
import FeatureExtractor.Types (Weather(..), EnrichedWeather(..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Csv (decodeByName)
import qualified Data.Vector as V

decodeCSV :: (MonadIO m) => ConduitT BS.ByteString Weather m ()
decodeCSV = do
  csvData <- CC.sinkLazy
  let decoded = decodeByName csvData
  case decoded of
    Left err -> do
      liftIO $ putStrLn err
    Right (_, v) -> do
      CC.yieldMany $ V.toList v

parallelWeatherChunks :: (MonadIO m) => ConduitT [Weather] [EnrichedWeather] m ()
parallelWeatherChunks = do
  rowData <- await
  case rowData of
    Nothing -> return ()
    Just w -> yield $ enrichWeather w

enrichWeather :: [Weather] -> [EnrichedWeather]
enrichWeather = undefined

run :: IO ()
run = do
  runConduitRes $
    CC.sourceFile "./data/raw/2019.csv"
      .| decodeCSV
      .| chunksOf 1000
      -- .| CC.filter precipitation -- this filter not is not going to work
      .| parallelWeatherChunks
      .| CC.sinkNull
