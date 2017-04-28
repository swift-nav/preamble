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

import Network.Socket.ByteString
import Preamble.Prelude
import Preamble.Types

sendto :: MonadStatsCtx c m => ByteString -> m ()
sendto metric = do
  s <- view scSocket
  a <- view scSockAddr
  liftIO $ void $ sendTo s metric a

stats :: (MonadStatsCtx c m, Show a) => Text -> Text -> a -> Tags -> m ()
stats stat name value tags = do
  let metric = name -:- show value -|- stat
      tagged = intercalate "," $ flip map tags $ uncurry (-:-)
  sendto $ encodeUtf8 $ bool (metric -|- "#" <> tagged) metric $ null tags

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
