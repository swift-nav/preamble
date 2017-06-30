{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | StatsD functionality.
--
module Preamble.Stats
  ( statsCount
  , statsGauge
  , statsHistogram
  , statsTimer
  , statsSet
  , statsIncrement
  , statsDecrement
  ) where

import qualified Data.Text        as T
import           Preamble.Prelude
import           Preamble.Types

stats :: (MonadStatsCtx c m, Show a) => Text -> Text -> a -> Tags -> m ()
stats metric name value tags = do
  labels <- (<> tags) <$> view scLabels
  prefix <- ap (`bool` mempty) T.null <$> view scPrefix
#if MIN_VERSION_basic_prelude(0,6,1)
  let statsd = prefix <> name -:- textFromString (show value) -|- metric
#else
  let statsd = prefix <> name -:- show value -|- metric
#endif
      tagged = T.intercalate "," $ flip map labels $ uncurry (-:-)
  stat <- view scStat
  liftIO $ stat $ encodeUtf8 $ bool (statsd -|- "#" <> tagged) statsd $ null labels

statsCount :: (MonadStatsCtx c m, Show a) => Text -> a -> Tags -> m ()
statsCount = stats "c"

statsGauge :: (MonadStatsCtx c m, Show a) => Text -> a -> Tags -> m ()
statsGauge = stats "g"

statsHistogram :: (MonadStatsCtx c m, Show a) => Text -> a -> Tags -> m ()
statsHistogram = stats "h"

statsTimer :: (MonadStatsCtx c m, Show a) => Text -> a -> Tags -> m ()
statsTimer = stats "ms"

statsSet :: (MonadStatsCtx c m, Show a) => Text -> a -> Tags -> m ()
statsSet = stats "s"

statsIncrement :: MonadStatsCtx c m => Text -> Tags -> m ()
statsIncrement name = stats "c" name (1 :: Int)

statsDecrement :: MonadStatsCtx c m => Text -> Tags -> m ()
statsDecrement name = stats "c" name (-1 :: Int)
