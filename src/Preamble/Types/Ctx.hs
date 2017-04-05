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
import Control.Monad.Reader
import Network.Socket
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
  , _cTrace    :: Trace
    -- ^ Configurable tracing function.
  }

$(makeClassy ''Ctx)

type MonadCtx c m =
  ( MonadMain m
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
  { _scCtx      :: Ctx
    -- ^ Parent environment.
  , _scSocket   :: Socket
    -- ^ UDP Socket for statsd.
  , _scSockAddr :: SockAddr
    -- ^ UDP SockAddr for statsd.
  }

$(makeClassyConstraints ''StatsCtx [''HasCtx])

instance HasCtx StatsCtx where
  ctx = scCtx

type MonadStatsCtx c m =
  ( MonadCtx c m
  , HasStatsCtx c
  )
