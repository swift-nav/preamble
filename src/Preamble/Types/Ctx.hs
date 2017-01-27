{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Context objects for monad transformers.
--
module Preamble.Types.Ctx
  ( module Preamble.Types.Ctx
  ) where

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Preamble.Prelude
import Preamble.Types.Alias

-- | Ctx
--
-- Base context, supports tracing.
--
data Ctx = Ctx
  { _cPreamble :: Pairs
    -- ^ Object to encode on every trace line.
  , _cTrace    :: Trace
    -- ^ Configurable tracing function.
  }

$(makeClassy ''Ctx)

type MonadCtx c m =
  ( MonadMain m
  , MonadReader c m
  , MonadLogger m
  , MonadCatch m
  , MonadThrow m
  , HasCtx c
  )
