{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Various alias types.
--
module Preamble.Types.Alias where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Aeson
import Preamble.Prelude

-- | Pairs
--
type Pairs = [(Text, Value)]

-- | Trace
--
type Trace = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | MonadControl
--
type MonadControl m =
  ( MonadBaseControl IO m
  , MonadIO m
  , MonadMask m
  )

-- | MonadMain
--
type MonadMain m =
  ( MonadControl m
  , MonadResource m
  )
