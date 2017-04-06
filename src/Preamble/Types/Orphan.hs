{-# OPTIONS  -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Various orphans.
--
module Preamble.Types.Orphan where

import Control.Monad.Random
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.UUID
import Preamble.Prelude

instance ToJSON UUID where
  toJSON = toJSON . toText

instance FromJSON UUID where
  parseJSON (String s) = maybe mzero return $ fromText s
  parseJSON _ = mzero

instance MonadRandom m => MonadRandom (ResourceT m) where
  getRandom   = lift getRandom
  {-# INLINE getRandom #-}
  getRandoms  = lift getRandoms
  {-# INLINE getRandomR #-}
  getRandomR  = lift . getRandomR
  {-# INLINE getRandoms #-}
  getRandomRs = lift . getRandomRs
  {-# INLINE getRandomRs #-}
