{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Monad transformer.
--
module Preamble.Types.Trans
  ( module Preamble.Types.Trans
  , runResourceT
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Preamble.Prelude

-- | Monad transformer for reading and logging.
--
newtype TransT c m a = TransT
  { unTransT :: LoggingT (ReaderT c m) a
    -- ^ LoggingT and ReaderT transformer.
  } deriving (Functor, Applicative, Monad, MonadLogger, MonadReader c, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadBase b m => MonadBase b (TransT c m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadBaseControl b m => MonadBaseControl b (TransT c m) where
    type StM (TransT c m) a = ComposeSt (TransT c) m a
    liftBaseWith = defaultLiftBaseWith
    {-# INLINE liftBaseWith #-}
    restoreM = defaultRestoreM
    {-# INLINE restoreM #-}

instance MonadTransControl (TransT c) where
  type StT (TransT c) a = StT (ReaderT c) a
  liftWith f = TransT $
    liftWith $ \g ->
      liftWith $ \h ->
        f (h . g . unTransT)
  {-# INLINE liftWith #-}
  restoreT = TransT . restoreT . restoreT
  {-# INLINE restoreT #-}

instance MonadTrans (TransT c) where
  lift = TransT . lift . lift
  {-# INLINE lift #-}

instance MonadResource m => MonadResource (TransT c m) where
  liftResourceT = lift . liftResourceT
  {-# INLINE liftResourceT #-}
