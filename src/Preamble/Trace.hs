{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tracing functionality around MonadLogger.
--
module Preamble.Trace
  ( newStderrTrace
  , newStdoutTrace
  , nullTrace
  , traceDebug
  , traceInfo
  , traceWarn
  , traceError
  , (.=)
  ) where

import           Control.Monad.Logger
import           Data.Aeson
import           Data.Aeson.Encode
import qualified Data.HashMap.Strict    as M
import           Data.Text              hiding (singleton)
import qualified Data.Text.Lazy         as LT
import           Data.Text.Lazy.Builder
import           Data.Time
import           Preamble.Prelude       hiding (null)
import           Preamble.Types
import           System.Log.FastLogger

-- | Trace out only if gte configured level.
--
levelTrace :: LogLevel -> LoggerSet -> Trace
levelTrace level ls _loc _source level' s =
  unless (level' < level) $ do
    pushLogStr ls s
    flushLogStr ls

-- | New logger to stderr.
--
newStderrTrace :: MonadIO m => LogLevel -> m Trace
newStderrTrace level =
  liftIO $ levelTrace level <$> newStderrLoggerSet defaultBufSize

-- | New logger to stdout.
--
newStdoutTrace :: MonadIO m => LogLevel -> m Trace
newStdoutTrace level =
  liftIO $ levelTrace level <$> newStdoutLoggerSet defaultBufSize

-- | Logger to nowhere.
--
nullTrace :: Trace
nullTrace _loc _source _level _s =
  return ()

-- | Trace out event with preamble and timestamp.
--
trace :: MonadCtx c m => (Text -> m ()) -> Text -> Pairs -> m ()
trace logN e ps = do
  p <- view cPreamble
  t <- liftIO getCurrentTime
  let preamble = bool [ "event" .= e, "time" .= t ] [ "time" .= t ] $ null e
  logN $ LT.toStrict $ toLazyText $ (<> singleton '\n') $
    encodeToTextBuilder $ Object $ M.fromList $ preamble <> p <> ps

-- | Debug tracing.
--
traceDebug :: MonadCtx c m => Text -> Pairs -> m ()
traceDebug = trace logDebugN

-- | Info tracing.
--
traceInfo :: MonadCtx c m => Text -> Pairs -> m ()
traceInfo = trace logInfoN

-- | Warn tracing.
--
traceWarn :: MonadCtx c m => Text -> Pairs -> m ()
traceWarn = trace logWarnN

-- | Error tracing.
--
traceError :: MonadCtx c m => Text -> Pairs -> m ()
traceError = trace logErrorN
