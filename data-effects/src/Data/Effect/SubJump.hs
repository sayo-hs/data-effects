{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

module Data.Effect.SubJump (
    module Data.Effect.SubJump,
    SubJump (..),
    callCC,
    sub,
)
where

import Control.Effect (callCC, sub)
import Data.Effect (SubJump (Jump, SubFork))
import Data.Function (fix)

makeEffectF' (def & noGenerateLabel & noGenerateOrderInstance) ''SubJump

getCC :: (SubJump ref :> es, Monad (Eff ff es), Free c ff) => Eff ff es (Eff ff es a)
getCC = callCC $ pure . fix
{-# INLINE getCC #-}
