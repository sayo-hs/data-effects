-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{-
The code before modification is BSD3 licensed, (c) 2010-2011 Patrick Bahr.

This fork was made to work around the problem that the
'Control.Effect.Class.Machinery.TH.makeEffect' function that generates multiple
definitions at once for convenience is not possible with only the original
'Data.Comp.Multi.Derive.makeHFunctor' function due to TH limitations,
because the original function takes the name of the data type as an argument,
but there is no version that takes 'DataInfo' as an argument (the data type
reification and the HFunctor derivation process are not separated as functions).
-}

{- |
Copyright   :  (c) 2010-2011 Patrick Bahr
               (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides @TemplateHaskell@ functions to derive an instance of
'Data.Comp.Multi.HFunctor.HFunctor'.

The definitions come from "Data.Comp.Multi.Derive" in the compdata-0.13.0 package.
-}
module Data.Effect.Class.TH.HFunctor where

import Data.Effect.Class.TH.HFunctor.Internal (abstractNewtype, deriveHFunctor)
import Language.Haskell.TH (Dec, Name, Q, reify)

{- |
Derive an instance of 'Data.Comp.Multi.HFunctor.HFunctor' for a type constructor of any higher-order
kind taking at least two arguments.
-}
makeHFunctor :: Name -> Q [Dec]
makeHFunctor fname = do
    Just dInfo <- abstractNewtype <$> reify fname
    deriveHFunctor dInfo
