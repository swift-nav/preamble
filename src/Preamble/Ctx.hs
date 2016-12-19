{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Preamble.Ctx
  ( runTransT
  , runCtx
  , preCtx
  ) where

import Control.Monad.Logger
import Control.Monad.Reader
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
runCtx :: MonadIO m => TransT Ctx m a -> m a
runCtx action = do
  t <- liftIO $ newStderrTrace LevelInfo
  runTransT (Ctx mempty t) action

-- | Update base context's preamble.
--
preCtx :: MonadCtx c m => Pairs -> TransT Ctx m a -> m a
preCtx preamble action = do
  c <- view ctx <&> cPreamble <>~ preamble
  runTransT c action

