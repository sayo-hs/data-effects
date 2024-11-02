{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects that can accumulate values monoidally in a context.
-}
module Data.Effect.Writer where

-- | An effect that can accumulate values monoidally in a context.
data Tell w a where
    -- | Accumulates new values to the cumulative value held in the context.
    Tell :: w -> Tell w ()

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

makeEffect [''Tell] [''WriterH]

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
pass :: (Tell w <: m, WriterH w <<: m, Monad m) => m (w -> w, a) -> m a
pass m = do
    (w, (f, a)) <- listen m
    tell $ f w
    pure a
