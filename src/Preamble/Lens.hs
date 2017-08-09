{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | makeClassy with constraints.
--
module Preamble.Lens
  ( makeClassyConstraints
  ) where

import Language.Haskell.TH
import Preamble.Prelude

-- | makeClassy with list of class constraints.
--
makeClassyConstraints :: Name -> [Name] -> DecsQ
makeClassyConstraints name names = do
  decls <- makeClassy name
  pure $ addConstraints names decls

-- | Add constaints to a class.
--
addConstraints :: [Name] -> [Dec] -> [Dec]
addConstraints names = \case
  ClassD cs n tvs f d : ds ->
    ClassD (newConstraints names tvs ++ cs) n tvs f d : ds
  ds -> ds

-- | Create new constaints.
--
newConstraints :: [Name] -> [TyVarBndr] -> [Type]
newConstraints ns =
  loop where
    loop = \case
      PlainTV name : _tvs ->
        flip map ns $ \n ->
          AppT (ConT n) (VarT name)
      _tv : tvs -> loop tvs
      [] -> []
