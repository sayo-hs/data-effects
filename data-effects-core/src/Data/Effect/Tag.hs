-- SPDX-License-Identifier: MPL-2.0
{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Tag where

import Data.Comp.Multi.HFunctor (HFunctor)
import Data.Effect (Effect, FirstOrder, OrderOf)

-- | Tagged effect.
newtype Tagged (e :: Effect) tag f a = Tagged {unTagged :: e f a}
    deriving stock (Functor, Foldable, Traversable)
    deriving newtype (HFunctor)

type instance OrderOf (Tagged e tag) = OrderOf e
instance (FirstOrder e) => FirstOrder (Tagged e tag)

-- | Tagged effect.
type (#) = Tagged

infixl 8 #

-- | Tagged effect.
pattern Tag :: forall tag e f a. e f a -> Tagged e tag f a
pattern Tag e = Tagged e

{-# COMPLETE Tag #-}
