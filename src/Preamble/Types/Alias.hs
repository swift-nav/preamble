{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Various alias types.
--
module Preamble.Types.Alias
  ( module Preamble.Types.Alias
  ) where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Aeson
import Preamble.Prelude

-- | Pairs
--
type Pairs = [(Text, Value)]

-- | Tags
--
type Tags = [(Text, Text)]

-- | Logger
--
type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | Stat
--
type Stat = ByteString -> IO ()

-- | MonadControl
--
type MonadControl m =
  ( MonadBaseControl IO m
  , MonadIO m
  , MonadMask m
  )
