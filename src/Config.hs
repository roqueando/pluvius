{-# LANGUAGE GADTs #-}

module Config where

import Core.DataPipeline

data ConfigT where
  ConfigT :: DataPipeline a => { database :: a } -> ConfigT

