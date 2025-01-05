{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects that can be used to hold environmental values in the context.
Environmental values are immutable and do not change across procedures, but you
can modify the value within a local scope using the `local` operation.
-}
module Data.Effect.Reader where

-- | An effect that holds a value of type @r@ in the context (environment).
data Ask r :: Effect where
    -- | Obtain a value from the environment.
    Ask :: Ask r f r

-- | An effect that locally modifies the value held in the environment.
data Local r :: Effect where
    -- | Locally modifies the value held in the environment.
    Local
        :: (r -> r)
        -- ^ A function that transforms the original value to the modified value.
        -> f a
        -- ^ The local scope where the modification is applied.
        -> Local r f a

makeEffectF ''Ask
makeEffectH ''Local

-- | Obtains a value from the environment and returns it transformed by the given function.
asks
    :: forall r es ff a c
     . (Ask r :> es, Functor (Eff ff es), Free c ff)
    => (r -> a)
    -> Eff ff es a
asks f = f <$> ask
{-# INLINE asks #-}

-- | Interpret the t'Ask'/t'Local' effects.
runReader
    :: (Free c ff, forall f. (c (ff f)) => Applicative (ff f))
    => r
    -> Eff ff (Local r ': Ask r ': es) a
    -> Eff ff es a
runReader r = runAsk r . runLocal
{-# INLINE runReader #-}

-- | Interpret the t'Ask' effect.
runAsk
    :: forall r es ff a c
     . (Free c ff, Applicative (Eff ff es))
    => r
    -> Eff ff (Ask r ': es) a
    -> Eff ff es a
runAsk r = interpret \Ask -> pure r
{-# INLINE runAsk #-}

-- | Interpret the t'Local' effect.
runLocal
    :: forall r es ff a c
     . (Free c ff, Applicative (Eff ff es), Ask r :> es)
    => Eff ff (Local r ': es) a
    -> Eff ff es a
runLocal = interpret handleLocal
{-# INLINE runLocal #-}

-- | A handler for the t'Local' effect.
handleLocal
    :: forall r es ff c
     . (Free c ff, Applicative (Eff ff es), Ask r :> es)
    => Local r ~~> Eff ff es
handleLocal (Local f a) = a & interpose @(Ask r) \Ask -> f <$> ask
{-# INLINE handleLocal #-}
