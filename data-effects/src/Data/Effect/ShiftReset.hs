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
    => ((a -> Eff ff es ans) -> Eff ff es ans)
    -> Eff ff es a
shift initiate = ushift \k detach -> detach $ initiate $ embShift . k
{-# INLINE shift #-}

ushift
    :: forall a ans es es' ff c
     . (Shift ff ans es' :> es, forall f. (c (ff f)) => Monad (ff f), Free c ff)
    => ( (a -> Eff ff (Shift ff ans es' ': es') ans)
         -> (Eff ff es ~> Eff ff (Shift ff ans es' ': es'))
         -> Eff ff (Shift ff ans es' ': es') ans
       )
    -> Eff ff es a
ushift initiate =
    unliftShift \k detach ->
        ShiftC $ initiate (unShiftC . k) (unShiftC . detach)
{-# INLINE ushift #-}

callCC
    :: forall a ans es es' ff c
     . (Shift ff ans es' :> es, forall f. (c (ff f)) => Monad (ff f), Free c ff)
    => ((a -> Eff ff es ans) -> Eff ff es a)
    -> Eff ff es a
callCC f = ushift \k detach -> detach (f $ embShift . k >=> abort) >>= k
{-# INLINE callCC #-}

ucallCC
    :: forall a ans es es' ff c
     . (Shift ff ans es' :> es, forall f. (c (ff f)) => Monad (ff f), Free c ff)
    => ((a -> Eff ff (Shift ff ans es' ': es') ans) -> Eff ff es a)
    -> Eff ff es a
ucallCC f = ushift \k detach -> detach (f $ k >=> abort) >>= k
{-# INLINE ucallCC #-}

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
    => Eff ff es (Eff ff es ans)
getCC = callCC \exit' -> let a = exit' a in pure a
{-# INLINE getCC #-}

ugetCC
    :: forall ans es es' ff c
     . (Shift ff ans es' :> es, forall f. (c (ff f)) => Monad (ff f), Free c ff)
    => Eff ff es (Eff ff (Shift ff ans es' ': es') ans)
ugetCC = ucallCC \exit' -> let a = exit' a in pure a
{-# INLINE ugetCC #-}

embShift
    :: forall ans a es es' ff c
     . (Shift ff ans es' :> es, Monad (Eff ff (Shift ff ans es' ': es')), Free c ff)
    => Eff ff (Shift ff ans es' ': es') a
    -> Eff ff es a
embShift m = unliftShift \k _ -> ShiftC $ m >>= unShiftC . k
{-# INLINE embShift #-}

data Reset :: Effect where
    Reset :: m a -> Reset m a
makeEffectH ''Reset
