{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Preamble.Ctx
  ( runTransT
  , runCtx
  , preCtx
  , runStatsCtx
  , preStatsCtx
  ) where

import Control.Monad.Logger
import Control.Monad.Reader
import Network.Socket
import Preamble.Prelude
import Preamble.Trace
import Preamble.Types

-- | Run monad transformer, picking up logger from context.
--
runTransT :: HasCtx c => c -> TransT c m a -> m a
runTransT c m =
  runReaderT (runLoggingT (unTransT m) (c ^. cTrace)) c

-- | Run base context.
--
runCtx :: MonadIO m => LogLevel -> TransT Ctx m a -> m a
runCtx level action = do
  t <- liftIO $ newStderrTrace level
  runTransT (Ctx mempty t) action

-- | Update base context's preamble.
--
preCtx :: MonadCtx c m => Pairs -> TransT Ctx m a -> m a
preCtx preamble action = do
  c <- view ctx <&> cPreamble <>~ preamble
  runTransT c action

-- | Run stats context.
--
runStatsCtx :: MonadCtx c m => String -> TransT StatsCtx m a -> m a
runStatsCtx host action = do
  c <- view ctx
  s <- liftIO $ socket AF_INET Datagram defaultProtocol
  a <- liftIO $ inet_addr host
  let sa = SockAddrInet 8125 a
  runTransT (StatsCtx c s sa) action

-- | Update stats context's preamble.
--
preStatsCtx :: MonadStatsCtx c m => Pairs -> TransT StatsCtx m a -> m a
preStatsCtx preamble action = do
  c <- view statsCtx <&> cPreamble <>~ preamble
  runTransT c action
