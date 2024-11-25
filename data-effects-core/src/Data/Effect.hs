{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect where

import Data.Coerce (Coercible)
import Data.Kind (Type)

-- | The kind for effects.
type Effect = (Type -> Type) -> Type -> Type

-- | An order of effect.
data EffectOrder = FirstOrder | HigherOrder
    deriving (Show, Eq, Ord)

type family OrderOf (e :: Effect) :: EffectOrder

class
    ( OrderOf e ~ 'FirstOrder
    , forall f g a. Coercible (e f a) (e g a)
    ) =>
    FirstOrder (e :: Effect)

-- | A effect with no operations.
data Nop :: Effect
    deriving anyclass (FirstOrder)

type instance OrderOf Nop = 'FirstOrder
