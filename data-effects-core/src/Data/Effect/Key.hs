-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Key where

import Data.Comp.Multi.HFunctor (HFunctor)
import Data.Effect (Effect, FirstOrder, OrderOf)

-- | Keyed effect.
newtype Keyed key (e :: Effect) f a = Key {unKeyed :: e f a}
    deriving stock (Functor, Foldable, Traversable)
    deriving newtype (HFunctor)

type instance OrderOf (Keyed key e) = OrderOf e
instance (FirstOrder e) => FirstOrder (Keyed key e)

-- | Keyed effect.
type (#>) = Keyed

infixr 7 #>
