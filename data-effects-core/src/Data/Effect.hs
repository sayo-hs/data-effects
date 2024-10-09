-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
               (c) 2023 Casper Bach Poulsen and Cas van der Rest
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect where

import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Kind (Type)

-- | The kind of first-order effects.
type EffectF = Type -> Type

-- | The kind of higher-order effects.
type EffectH = (Type -> Type) -> Type -> Type

{- | Lift first-order effects to higher-order effects.

     Come from [heft-lang\/POPL2023\/haskell\/src\/Elab.hs]
    (https://github.com/heft-lang/POPL2023/blob/74afe1d5ce0b491cffe40cc5c73a2a5ee6a94d9c/haskell/src/Elab.hs#L9-L10).
-}
newtype LiftFOE (ins :: EffectF) (f :: Type -> Type) a = LiftFOE {unliftFOE :: ins a}
    deriving stock (Functor, Foldable, Traversable)

instance HFunctor (LiftFOE ins) where
    hfmap _ (LiftFOE e) = LiftFOE e
    {-# INLINE hfmap #-}

-- | A first-order effect with no operations.
data Nop :: EffectF

-- | A higher-order effect with no operations.
type LNop = LiftFOE Nop
