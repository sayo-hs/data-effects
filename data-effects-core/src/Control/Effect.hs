-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect where

import Data.Effect (EffectF, EffectH, LiftFOE (unliftFOE))
import Data.Kind (Type)

-- | A type class that represents the ability to send an first-order effect @ins@ to carrier @f@.
class SendFOE (ins :: EffectF) f where
    -- | Send an /instruction/ @ins@ to carrier @f@.
    sendFOE :: ins a -> f a

-- | The operator version of `SendFOE`.
type (<:) = SendFOE

infix 2 <:

-- | A type class that represents the ability to send a higher-order effect @sig@ to carrier @f@.
class SendHOE (sig :: EffectH) f where
    -- | Send a higher-order effect @sig@ to carrier @f@.
    sendHOE :: sig f a -> f a

-- | The operator version of `SendHOE`.
type (<<:) = SendHOE

infix 2 <<:

instance (SendFOE ins f) => SendHOE (LiftFOE ins) f where
    sendHOE = sendFOE . unliftFOE
    {-# INLINE sendHOE #-}

-- | A natural transformation.
type f ~> g = forall (x :: Type). f x -> g x

infixr 2 ~>
