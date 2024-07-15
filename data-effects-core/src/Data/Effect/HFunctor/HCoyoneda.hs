{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Free `HFunctor`s.
-}
module Data.Effect.HFunctor.HCoyoneda where

import Control.Effect (type (~>))
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Kind (Type)

-- | The free `HFunctor` for a @ff@.
data HCoyoneda ff f (a :: Type) = forall g. HCoyoneda (g ~> f) (ff g a)

deriving stock instance (forall g. Functor (ff g)) => Functor (HCoyoneda ff f)
deriving stock instance (forall g. Foldable (ff g)) => Foldable (HCoyoneda ff f)
deriving stock instance (forall g. Traversable (ff g)) => Traversable (HCoyoneda ff f)

instance HFunctor (HCoyoneda ff) where
    hfmap phi (HCoyoneda f ff) = HCoyoneda (phi . f) ff
    {-# INLINE hfmap #-}

liftHCoyoneda :: ff f a -> HCoyoneda ff f a
liftHCoyoneda = HCoyoneda id
{-# INLINE liftHCoyoneda #-}

lowerHCoyoneda :: HFunctor ff => HCoyoneda ff f a -> ff f a
lowerHCoyoneda (HCoyoneda f ff) = hfmap f ff
{-# INLINE lowerHCoyoneda #-}

hoistHCoyoneda :: (forall g. ff g ~> gg g) -> HCoyoneda ff f a -> HCoyoneda gg f a
hoistHCoyoneda f (HCoyoneda g ff) = HCoyoneda g (f ff)
{-# INLINE hoistHCoyoneda #-}

hCoyoneda :: (forall g. g ~> f -> ff g a -> r) -> HCoyoneda ff f a -> r
hCoyoneda f (HCoyoneda k ff) = f k ff
{-# INLINE hCoyoneda #-}

hCoyoneda_ :: (forall g. ff g a -> r) -> HCoyoneda ff f a -> r
hCoyoneda_ f (HCoyoneda _ ff) = f ff
{-# INLINE hCoyoneda_ #-}

newtype (~~>) ff g f (a :: Type) = Interpret ((f ~> g) -> ff g a)
