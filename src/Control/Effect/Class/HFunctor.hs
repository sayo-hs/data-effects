{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at https://mozilla.org/MPL/2.0/.           -}

module Control.Effect.Class.HFunctor where

import Control.Effect.Class (LiftIns (LiftIns), unliftIns)

{- | The class for /signature/s (datatypes of higher-order effect).

     Come from [heft-lang\/POPL2023\/haskell\/src\/Hefty.hs]
    (https://github.com/heft-lang/POPL2023/blob/74afe1d5ce0b491cffe40cc5c73a2a5ee6a94d9c/haskell/src/Hefty.hs#L9-L10).
-}
class HFunctor h where
    -- | Hoist the monad underlying a /signature/.
    hmap :: (forall x. f x -> g x) -> h f a -> h g a

instance HFunctor (LiftIns ins) where
    hmap _ = LiftIns . unliftIns
