module FeatureExtractor.ExtractCSV where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Combinators as CC

import Control.Parallel.Strategies (parListChunk, using, rpar)
import Data.Conduit.List (chunksOf)
import Data.Conduit (ConduitT, runConduitRes, (.|))
import FeatureExtractor.Types (Weather(..), EnrichedWeather(..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Csv (decodeByName, encode)
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

parallelWeatherChunks :: [Weather] -> [EnrichedWeather]
parallelWeatherChunks chunk = map enrichWeather chunk `using` parListChunk 10 rpar

enrichWeather :: Weather -> EnrichedWeather
enrichWeather = undefined

encodeChunk :: [EnrichedWeather] -> BL.ByteString
encodeChunk results = encode (map (\x -> [x :: EnrichedWeather]) results)

run :: IO ()
run = do
  runConduitRes $
    CC.sourceFile "./data/raw/2019.csv"
      .| decodeCSV
      .| chunksOf 1000
      -- .| CC.filter precipitation -- this filter not is not going to work
      .| CC.map parallelWeatherChunks
      .| CC.map encodeChunk
      .| CC.map BL.toStrict
      .| CC.sinkFile "./data/processed/enriched_2019.csv"
