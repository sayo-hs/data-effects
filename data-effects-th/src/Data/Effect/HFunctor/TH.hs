{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides @TemplateHaskell@ functions to derive an instance of
 t'Data.Effect.HFunctor.HFunctor'.
-}
module Data.Effect.HFunctor.TH where

import Data.Effect.HFunctor.TH.Internal (deriveHFunctor)
import Data.Effect.TH.Internal (analyzeData)
import Data.Functor ((<&>))
import Data.List.Infinite (Infinite)
import Language.Haskell.TH (Dec, Name, Q, reify)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (nameBase)

{- |
Derive an instance of t'Data.Effect.HFunctor.HFunctor' for a type constructor of any higher-order
kind taking at least two arguments.
-}
makeHFunctor :: Name -> Q [Dec]
makeHFunctor name = makeHFunctor' name (const [t|()|])

{- |
Derive an instance of t'Data.Effect.HFunctor.HFunctor' for a type constructor of any higher-order
kind taking at least two arguments.

Furthermore, you can manually provide type constraints for the instance:

@
{\-# LANGUAGE BlockArguments #-\}
import Data.List.Infinite (Infinite ((:<)))

data Example (g :: Type -> Type) h (f :: Type -> Type) (a :: Type) where
    Example :: g (h f a) -> Example g h f a

makeHFunctor' ''Example \\(g \:\< h \:\< _) -> [t| (Functor $g, HFunctor $h) |]
@
-}
makeHFunctor' :: Name -> (Infinite (Q TH.Type) -> Q TH.Type) -> Q [Dec]
makeHFunctor' name manualCxt = do
    reify name <&> analyzeData >>= \case
        Just dat -> do
            deriveHFunctor manualCxt dat
        Nothing -> fail $ "The specified name is not that of a data type: " ++ nameBase name
