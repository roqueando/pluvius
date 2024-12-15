module FeatureExtractor.ExtractCSV where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Parallel.Strategies (parListChunk, rpar, using)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (ConduitT, runConduitRes, (.|))
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.List (chunksOf)
import Data.Csv (decodeByName, encode)
import qualified Data.Vector as V
import FeatureExtractor.Types (EnrichedWeather (..), Weather (..))

decodeCSV :: (MonadIO m) => ConduitT BS.ByteString Weather m ()
decodeCSV = do
  csvData <- CC.sinkLazy
  let decoded = decodeByName csvData
  case decoded of
    Left err -> do
      liftIO $ putStrLn err
    Right (_, v) -> do
      CC.yieldMany $ V.toList v

parallelWeatherChunks :: [Weather] -> [EnrichedWeather]
parallelWeatherChunks chunk = map enrichWeather chunk `using` parListChunk 100 rpar

enrichWeather :: Weather -> EnrichedWeather
enrichWeather w = undefined

encodeChunk :: [EnrichedWeather] -> BL.ByteString
encodeChunk = encode

run :: IO ()
run = do
  runConduitRes $
    CC.sourceFile "./data/raw/2019_test.csv"
      .| decodeCSV
      .| chunksOf 1000
      .| CC.map parallelWeatherChunks
      .| CC.map encodeChunk
      .| CC.map BL.toStrict
      .| CC.sinkFile "./data/processed/enriched_2019_test.csv"
