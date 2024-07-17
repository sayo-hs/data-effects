-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
               (c) 2023 Casper Bach Poulsen and Cas van der Rest
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect where

import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Kind (Type)

-- | A kind of /signature class/ (a datatype of higher-order effect).
type SigClass = (Type -> Type) -> Type -> Type

-- | A kind of /instruction class/ (a datatype of first-order effect).
type InsClass = Type -> Type

{- | Lift an /instruction class/ to a /signature class/.

     Come from [heft-lang\/POPL2023\/haskell\/src\/Elab.hs]
    (https://github.com/heft-lang/POPL2023/blob/74afe1d5ce0b491cffe40cc5c73a2a5ee6a94d9c/haskell/src/Elab.hs#L9-L10).
-}
newtype LiftIns (ins :: InsClass) (f :: Type -> Type) a = LiftIns {unliftIns :: ins a}
    deriving stock (Functor, Foldable, Traversable)

instance HFunctor (LiftIns ins) where
    hfmap _ (LiftIns e) = LiftIns e
    {-# INLINE hfmap #-}

-- | An /instruction class/ with no effects.
data Nop :: InsClass

-- | A /signature class/ with no effects.
type LNop = LiftIns Nop
