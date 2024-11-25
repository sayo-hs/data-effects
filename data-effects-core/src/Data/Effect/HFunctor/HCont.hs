{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.HFunctor.HCont where

import Control.Effect (type (~>))
import Data.Effect (EffectOrder (HigherOrder), OrderOf)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Kind (Type)

-- | This represents that the effect @ff@ is finally interpreted as the base carrier @b@.
newtype HCont ff b f (a :: Type) = HCont {unHCont :: (f ~> b) -> ff b a}
    deriving stock (Functor)

type instance OrderOf (HCont ff b) = 'HigherOrder

instance HFunctor (HCont ff g) where
    hfmap phi (HCont f) = HCont \k -> f $ k . phi
    {-# INLINE hfmap #-}
