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

import Data.Effect (InsClass, LiftIns (unliftIns), SigClass)
import Data.Kind (Type)

-- | A type class that represents the ability to send an /instruction/ @ins@ to carrier @f@.
class SendIns (ins :: InsClass) f where
    -- | Send an /instruction/ @ins@ to carrier @f@.
    sendIns :: ins a -> f a

-- | The operator version of `SendIns`.
type (<:) = SendIns

infix 2 <:

-- | A type class that represents the ability to send a /signature/ @sig@ to carrier @f@.
class SendSig (sig :: SigClass) f where
    -- | Send a /signature/ @sig@ to carrier @f@.
    sendSig :: sig f a -> f a

-- | The operator version of `SendSig`.
type (<<:) = SendSig

infix 2 <<:

instance SendIns ins f => SendSig (LiftIns ins) f where
    sendSig = sendIns . unliftIns
    {-# INLINE sendSig #-}

-- | A natural transformation.
type f ~> g = forall (x :: Type). f x -> g x

infixr 2 ~>
