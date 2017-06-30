{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tracing functionality around MonadLogger.
--
module Preamble.Trace
  ( newStderrLogger
  , newStdoutLogger
  , nullLogger
  , traceDebug
  , traceInfo
  , traceWarn
  , traceError
  , (.=)
  ) where

import           Control.Monad.Logger
import           Data.Aeson
#if MIN_VERSION_aeson(1,0,2)
import           Data.Aeson.Text
#else
import           Data.Aeson.Encode
#endif
import qualified Data.HashMap.Strict    as M
import           Data.Text              hiding (singleton)
import qualified Data.Text.Lazy         as LT
import           Data.Text.Lazy.Builder
import           Data.Time
import           Preamble.Prelude       hiding (null)
import           Preamble.Types
import           System.Log.FastLogger

-- | Log out only if gte configured level.
--
logger :: LogLevel -> LoggerSet -> Logger
logger level ls _loc _source level' s =
  unless (level' < level) $ do
    pushLogStr ls s
    flushLogStr ls

-- | New logger to stderr.
--
newStderrLogger :: MonadIO m => LogLevel -> m Logger
newStderrLogger level = liftIO $ logger level <$> newStderrLoggerSet defaultBufSize

-- | New logger to stdout.
--
newStdoutLogger :: MonadIO m => LogLevel -> m Logger
newStdoutLogger level = liftIO $ logger level <$> newStdoutLoggerSet defaultBufSize

-- | Logger to nowhere.
--
nullLogger :: Logger
nullLogger _loc _source _level _s = return ()

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
