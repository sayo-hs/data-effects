{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

module Data.Effect.ShiftReset where

import Control.Monad ((>=>))

data UnliftShift (ans :: Type) b :: Effect where
    UnliftShift
        :: forall ans b m a
         . ((a -> b ans) -> (forall x. m x -> b x) -> b ans)
        -> UnliftShift ans b m a
makeEffectH' (def & noGenerateLabel) ''UnliftShift

data UnliftShiftLabel (ans :: Type)
type instance LabelOf (UnliftShift ans b) = UnliftShiftLabel ans

type Shift ff ans es = UnliftShift ans (ShiftC ff ans es)

newtype ShiftC ff ans es a
    = ShiftC {unShiftC :: Eff ff (Shift ff ans es ': es) a}

shift
    :: forall a ans es es' ff c
     . (Shift ff ans es' :> es, forall f. (c (ff f)) => Monad (ff f), Free c ff)
    => ( (a -> Eff ff (Shift ff ans es' ': es') ans)
         -> (Eff ff es ~> Eff ff (Shift ff ans es' ': es'))
         -> Eff ff (Shift ff ans es' ': es') ans
       )
    -> Eff ff es a
shift initiate =
    unliftShift \k run ->
        ShiftC $ initiate (unShiftC . k) (unShiftC . run)
{-# INLINE shift #-}

callCC
    :: forall a ans es es' ff c
     . (Shift ff ans es' :> es, forall f. (c (ff f)) => Monad (ff f), Free c ff)
    => ((a -> Eff ff (Shift ff ans es' ': es') ans) -> Eff ff es a)
    -> Eff ff es a
callCC f = shift \k run -> run (f $ k >=> run . abort) >>= k
{-# INLINE callCC #-}

abort
    :: forall a ans es es' ff c
     . (Shift ff ans es' :> es, Applicative (Eff ff (Shift ff ans es' ': es')), Free c ff)
    => ans
    -> Eff ff es a
abort ans = unliftShift \_ _ -> ShiftC $ pure ans
{-# INLINE abort #-}

getCC
    :: forall ans es es' ff c
     . (Shift ff ans es' :> es, forall f. (c (ff f)) => Monad (ff f), Free c ff)
    => Eff ff es (Eff ff (Shift ff ans es' ': es') ans)
getCC = callCC \exit' -> let a = exit' a in pure a
{-# INLINE getCC #-}

detachShift
    :: forall ans a es es' ff c
     . (Shift ff ans es' :> es, Monad (Eff ff (Shift ff ans es' ': es')), Free c ff)
    => Eff ff (Shift ff ans es' ': es') a
    -> Eff ff es a
detachShift m = unliftShift \k _ -> ShiftC $ m >>= unShiftC . k
{-# INLINE detachShift #-}

data Reset :: Effect where
    Reset :: m a -> Reset m a
makeEffectH ''Reset
