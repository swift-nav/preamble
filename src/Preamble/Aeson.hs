{-# LANGUAGE NoImplicitPrelude #-}

-- | Aeson generic deriving options.
--
module Preamble.Aeson
  ( camelOptions
  , snakeOptions
  , spinalOptions
  , maybeResult
  , eitherResult
  ) where

import Data.Aeson.Types
import Data.Char
import Data.Text
import Data.Text.Manipulate
import Preamble.Prelude     hiding (dropWhile)

-- | Remove characters up to the first upper case character.
--
unprefix :: Text -> Text
unprefix = dropWhile (not . isUpper)

-- | Derive fields with camelCase.
--
camelOptions :: Options
camelOptions = defaultOptions
  { fieldLabelModifier     = unpack . toCamel . unprefix . pack
  , constructorTagModifier = unpack . toCamel . unprefix . lowerHead . pack
  , omitNothingFields      = True
  }

-- | Derive fields with snake_case.
--
snakeOptions :: Options
snakeOptions = defaultOptions
  { fieldLabelModifier     = unpack . toSnake . unprefix . pack
  , constructorTagModifier = unpack . toSnake . unprefix . lowerHead . pack
  , omitNothingFields      = True
  }

-- | Derive fields with spinal-case.
--
spinalOptions :: Options
spinalOptions = defaultOptions
  { fieldLabelModifier     = unpack . toSpinal . unprefix . pack
  , constructorTagModifier = unpack . toSpinal . unprefix . lowerHead . pack
  , omitNothingFields      = True
  }

-- | Convert Aeson Result into a Maybe.
--
maybeResult :: Result a -> Maybe a
maybeResult (Error _s) = Nothing
maybeResult (Success a) = Just a

-- | Convert Aeson Result into an Either.
--
eitherResult :: Result a -> Either String a
eitherResult (Error s) = Left s
eitherResult (Success a) = Right a
