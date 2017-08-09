{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Local Prelude.
--
module Preamble.Prelude
  ( module Exports
  , maybe'
  , either'
  , maybe_
  , eitherThrowIO
  , eitherThrowIO'
  , maybeThrowIO
  , maybeThrowIO'
  , boolThrowIO
  , textFromString
  , textShow
  , (-/-)
  , (-|-)
  , (-.-)
  , (-:-)
  , (=.)
  ) where

import BasicPrelude as Exports hiding (bool)
import Control.Lens as Exports hiding (uncons, (.=), (<.>))
import Data.Bool    as Exports
import Data.Text
import Safe         as Exports (headMay, initMay, tailMay)

-- | maybe with hanging function.
--
maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m b a = maybe b a m

-- | either with hanging function.
--
either' :: Either a b -> (a -> c) -> (b -> c) -> c
either' e b a = either b a e

-- | Maybe that returns () if Nothing
--
maybe_ :: Applicative f => Maybe a -> (a -> f ()) -> f ()
maybe_ = flip $ maybe $ pure ()

-- | Throw Exception on either error.
--
eitherThrowIO :: (MonadIO m, Exception e) => Either e a -> m a
eitherThrowIO = either (liftIO . throwIO) pure

-- | Throw userError on either error.
--
eitherThrowIO' :: MonadIO m => Either String a -> m a
eitherThrowIO' = either (liftIO . throwIO . userError) pure

-- | Throw Exception on maybe nothing.
--
maybeThrowIO :: (MonadIO m, Exception e) => e -> Maybe a -> m a
maybeThrowIO e = maybe (liftIO $ throwIO e) pure

-- | Throw userError on maybe nothing.
--
maybeThrowIO' :: MonadIO m => String -> Maybe a -> m a
maybeThrowIO' s = maybe (liftIO $ throwIO $ userError s) pure

-- | Throw userError on false.
--
boolThrowIO :: MonadIO m => String -> Bool -> m ()
boolThrowIO = flip unless . liftIO . throwIO . userError

-- | Reverse of textToString
--
textFromString :: String -> Text
textFromString = pack

textShow :: Show a => a -> Text
textShow =
#if MIN_VERSION_basic_prelude(0,6,1)
  textFromString . show
#else
  show
#endif

-- | </> for IsString.
--
(-/-) :: (IsString s, Monoid s) => s -> s -> s
(-/-) = (<>) . (<> "/")

-- | <|> for IsString.
--
(-|-) :: (IsString s, Monoid s) => s -> s -> s
(-|-) = (<>) . (<> "|")

-- | <.> for IsString.
--
(-.-) :: (IsString s, Monoid s) => s -> s -> s
(-.-) = (<>) . (<> ".")

-- | <:> for IsString.
--
(-:-) :: (IsString s, Monoid s) => s -> s -> s
(-:-) = (<>) . (<> ":")

-- | For making tags.
--
(=.) :: a -> b -> (a, b)
(=.) = (,)
