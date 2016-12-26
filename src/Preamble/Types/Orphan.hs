{-# OPTIONS  -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Various orphans.
--
module Preamble.Types.Orphan where

import Data.Aeson
import Data.UUID
import Preamble.Prelude

instance ToJSON UUID where
  toJSON = toJSON . toText

instance FromJSON UUID where
  parseJSON (String s) = maybe mzero return $ fromText s
  parseJSON _ = mzero
