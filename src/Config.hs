{-# LANGUAGE GADTs #-}

module Config where

import Core.DataPipeline ( DataPipeline )

data ConfigT = forall a. (DataPipeline a) => ConfigT {database :: a}
