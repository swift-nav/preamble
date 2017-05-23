{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Context objects for monad transformers.
--
module Preamble.Types.Ctx
  ( module Preamble.Types.Ctx
  ) where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.Reader
import Preamble.Lens
import Preamble.Prelude
import Preamble.Types.Alias

-- | Ctx
--
-- Base context, supports tracing.
--
data Ctx = Ctx
  { _cPreamble :: Pairs
    -- ^ Object to encode on every trace line.
  , _cLogger   :: Logger
    -- ^ Configurable tracing function.
  }

$(makeClassy ''Ctx)

type MonadCtx c m =
  ( MonadMain m
  , MonadRandom m
  , MonadReader c m
  , MonadLogger m
  , MonadCatch m
  , MonadThrow m
  , HasCtx c
  )

-- | StatsCtx
--
-- Stats context.
--
data StatsCtx = StatsCtx
  { _scCtx    :: Ctx
    -- ^ Parent environment.
  , _scLabels :: Tags
    -- ^ Tags to append to every stat.
  , _scStat   :: Stat
    -- ^ Configurable stat function.
  , _scPrefix :: Text
    -- ^ Stats prefix.
  }

$(makeClassyConstraints ''StatsCtx [''HasCtx])

instance HasCtx StatsCtx where
  ctx = scCtx

type MonadStatsCtx c m =
  ( MonadCtx c m
  , HasStatsCtx c
  )
