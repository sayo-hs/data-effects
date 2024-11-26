-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Tag where

import Data.Comp.Multi.HFunctor (HFunctor)
import Data.Effect (Effect, FirstOrder, OrderOf)

-- | Tagged effect.
newtype Tagged tag (e :: Effect) f a = Tag {unTagged :: e f a}
    deriving stock (Functor, Foldable, Traversable)
    deriving newtype (HFunctor)

type instance OrderOf (Tagged tag e) = OrderOf e
instance (FirstOrder e) => FirstOrder (Tagged tag e)

-- | Tagged effect.
type e # tag = Tagged tag e

infixl 8 #
