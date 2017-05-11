{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Preamble.Ctx
  ( runTransT
  , runCtx
  , preCtx
  , runStatsCtx
  , preStatsCtx
  , labStatsCtx
  ) where

import Control.Monad.Logger
import Control.Monad.Reader
import Network.Socket            hiding (sendTo)
import Network.Socket.ByteString
import Preamble.Prelude
import Preamble.Trace
import Preamble.Types
import System.Environment

-- | Run monad transformer, picking up logger from context.
--
runTransT :: HasCtx c => c -> TransT c m a -> m a
runTransT c m =
  runReaderT (runLoggingT (unTransT m) (c ^. cLogger)) c

-- | Run base context.
--
runCtx :: MonadIO m => TransT Ctx m a -> m a
runCtx action = do
  t <- liftIO $ newStderrLogger LevelInfo
  runTransT (Ctx mempty t) action

-- | Update base context's preamble.
--
preCtx :: MonadCtx c m => Pairs -> TransT Ctx m a -> m a
preCtx preamble action = do
  c <- view ctx <&> cPreamble <>~ preamble
  runTransT c action

-- | Run stats context.
--
runStatsCtx :: MonadCtx c m => TransT StatsCtx m a -> m a
runStatsCtx action = do
  c <- view ctx
  s <- liftIO $ socket AF_INET Datagram defaultProtocol
  h <- liftIO $ fromMaybe "127.0.0.1" <$> lookupEnv "STATS_HOST"
  a <- liftIO $ inet_addr h
  let stat m = void $ sendTo s m $ SockAddrInet 8125 a
  runTransT (StatsCtx c mempty stat) action

-- | Update stats context's preamble.
--
preStatsCtx :: MonadStatsCtx c m => Pairs -> TransT StatsCtx m a -> m a
preStatsCtx preamble action = do
  c <- view statsCtx <&> cPreamble <>~ preamble
  runTransT c action

-- | Update stats context's labels.
--
labStatsCtx :: MonadStatsCtx c m => Tags -> TransT StatsCtx m a -> m a
labStatsCtx labels action = do
  c <- view statsCtx <&> scLabels <>~ labels
  runTransT c action
