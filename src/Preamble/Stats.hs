{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | StatsD functionality.
--
module Preamble.Stats
  ( statsCount
  , statsGauge
  , statsHistogram
  , statsTimer
  , statsSet
  ) where

import Network.Socket.ByteString
import Preamble.Prelude
import Preamble.Types

sendto :: MonadStatsCtx c m => ByteString -> m ()
sendto metric = do
  s <- view scSocket
  a <- view scSockAddr
  liftIO $ void $ sendTo s metric a

stats :: (MonadStatsCtx c m, Num a, Show a) => Text -> Text -> a -> Tags -> m ()
stats stat name value tags = do
  let metric = name -:- show value -|- stat
      tagged = intercalate "," $ flip map tags $ uncurry (-:-)
  sendto $ encodeUtf8 $ bool (metric -|- "#" <> tagged) metric $ null tags

statsCount :: (MonadStatsCtx c m, Num a, Show a) => Text -> a -> Tags -> m ()
statsCount = stats "c"

statsGauge :: (MonadStatsCtx c m, Num a, Show a) => Text -> a -> Tags -> m ()
statsGauge = stats "g"

statsHistogram :: (MonadStatsCtx c m, Num a, Show a) => Text -> a -> Tags -> m ()
statsHistogram = stats "h"

statsTimer :: (MonadStatsCtx c m, Num a, Show a) => Text -> a -> Tags -> m ()
statsTimer = stats "ms"

statsSet :: (MonadStatsCtx c m, Num a, Show a) => Text -> a -> Tags -> m ()
statsSet = stats "s"
