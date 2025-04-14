-- SPDX-License-Identifier: MPL-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Shift where

import Data.Effect (Shift)
import Data.Function (fix)

makeEffectF_' (def & noGenerateLabel & noGenerateOrderInstance) ''Shift

subShift
    :: forall ans ref a b es ff c
     . (Shift ans ref :> es, Monad (Eff ff es), Free c ff)
    => (ref a -> Eff ff es b)
    -> (a -> Eff ff es b)
    -> Eff ff es b
subShift p q = subShiftFork >>= either p q
{-# INLINE subShift #-}

shift
    :: forall a ans ref es ff c
     . (Shift ans ref :> es, Monad (Eff ff es), Free c ff)
    => ((a -> Eff ff es ans) -> Eff ff es a)
    -> Eff ff es a
shift f = subShift (f . call) pure
{-# INLINE shift #-}

getShiftCC
    :: forall ans ref es ff c
     . (Shift ans ref :> es, Monad (Eff ff es), Free c ff)
    => Eff ff es (Eff ff es ans)
getShiftCC = shift $ pure . fix
{-# INLINE getShiftCC #-}
