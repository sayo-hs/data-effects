{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.SubJump (
    module Data.Effect.SubJump,
    SubJump (..),
    callCC_,
    sub,
)
where

import Control.Effect (callCC_, sub)
import Data.Effect (SubJump (Jump, SubFork))
import Data.Function (fix)

makeEffectF' (def & noGenerateLabel & noGenerateOrderInstance) ''SubJump

callCC
    :: forall ref a es ff c
     . (SubJump ref :> es, Monad (Eff ff es), Free c ff)
    => (forall b. (a -> Eff ff es b) -> Eff ff es a)
    -> Eff ff es a
callCC f = sub (f . jump) pure
{-# INLINE callCC #-}

getCC
    :: forall ref a es ff c
     . (SubJump ref :> es, Monad (Eff ff es), Free c ff)
    => Eff ff es (Eff ff es a)
getCC = callCC_ $ pure . fix
{-# INLINE getCC #-}
