{-# LANGUAGE GADTs #-}

module Core.Entity.Settings where

import Core.Adapter.Pipeline ( Pipeline )

data SettingsT = forall a. (Pipeline a) => SettingsT {database :: a}
