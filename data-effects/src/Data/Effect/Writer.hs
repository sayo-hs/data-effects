{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects that can accumulate values monoidally in a context.
-}
module Data.Effect.Writer where

-- | An effect that can accumulate values monoidally in a context.
data Tell w :: Effect where
    -- | Accumulates new values to the cumulative value held in the context.
    Tell :: w -> Tell w f ()

-- | An effect that performs local operations on accumulations in the context on a per-scope basis.
data WriterH w f a where
    -- | Obtains the accumulated value in the scope and returns it together as a pair.
    Listen
        :: f a
        -- ^ The scope from which to obtain the accumulation.
        -> WriterH w f (w, a)
    -- | Modifies the accumulation in the scope based on the given function.
    Censor
        :: (w -> w)
        -- ^ A function for modifying the accumulated value.
        -> f a
        -- ^ The scope where the modification is applied.
        -> WriterH w f a

makeEffectF ''Tell
makeEffectH ''WriterH

{- |
For a given scope, uses the function (the first component of the pair returned
by that scope) to modify the accumulated value of that scope, and then
accumulates the result into the current outer scope.

@
pass m = do
    (w, (f, a)) <- listen m
    tell $ f w
    pure a
@
-}
pass
    :: (Tell w :> es, WriterH w :> es, Monad (Eff ff es), Free c ff)
    => Eff ff es (w -> w, a)
    -> Eff ff es a
pass m = do
    (w, (f, a)) <- listen m
    tell $ f w
    pure a
{-# INLINE pass #-}

-- | 'censor' with pre-applying semantics.
censorPre
    :: forall w es ff a c
     . (Tell w :> es, Monoid w, Free c ff)
    => (w -> w)
    -> Eff ff es a
    -> Eff ff es a
censorPre f = interpose @(Tell w) \(Tell w) -> tell $ f w
{-# INLINE censorPre #-}
