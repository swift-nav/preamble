{-# OPTIONS  -fno-warn-orphans #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Various orphans.
--
module Preamble.Types.Orphan where

import Control.Monad.Random
import Control.Monad.Trans.Resource
#if MIN_VERSION_aeson(1,1,0)
#else
import Data.Aeson
import Data.UUID
#endif
import Preamble.Prelude

#if MIN_VERSION_aeson(1,1,0)
#else
instance ToJSON UUID where
  toJSON = toJSON . toText

instance FromJSON UUID where
  parseJSON (String s) = maybe mzero pure $ fromText s
  parseJSON _ = mzero
#endif

instance MonadRandom m => MonadRandom (ResourceT m) where
  getRandom   = lift getRandom
  {-# INLINE getRandom #-}
  getRandoms  = lift getRandoms
  {-# INLINE getRandomR #-}
  getRandomR  = lift . getRandomR
  {-# INLINE getRandoms #-}
  getRandomRs = lift . getRandomRs
  {-# INLINE getRandomRs #-}
