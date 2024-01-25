-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides @TemplateHaskell@ functions to derive an instance of
'Data.Comp.Multi.HFunctor.HFunctor'.

The definitions come from "Data.Comp.Multi.Derive" in the compdata-0.13.0 package.
-}
module Data.Effect.HFunctor.TH where

import Data.Effect.HFunctor.TH.Internal (deriveHFunctor)
import Data.Effect.TH.Internal (analyzeData)
import Language.Haskell.TH (Dec, Name, Q, reify)

{- |
Derive an instance of 'Data.Comp.Multi.HFunctor.HFunctor' for a type constructor of any higher-order
kind taking at least two arguments.
-}
makeHFunctor :: Name -> Q [Dec]
makeHFunctor name = do
    Just info <- analyzeData <$> reify name
    deriveHFunctor info
