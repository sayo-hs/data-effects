-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect where

import Data.Effect (Effect)
import Data.Kind (Type)

-- | A type class that represents the ability to perform an effect @e@ on carrier the @f@.
class Perform (e :: Effect) f where
    -- | Perform an effect @e@ on the carrier @f@.
    perform :: e f a -> f a

type (<!) = Perform
infix 2 <!

-- | A natural transformation.
type f ~> g = forall (x :: Type). f x -> g x

infixr 2 ~>
