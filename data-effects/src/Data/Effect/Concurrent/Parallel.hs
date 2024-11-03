{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects for parallel computations.
-}
module Data.Effect.Concurrent.Parallel where

#if ( __GLASGOW_HASKELL__ < 906 )
import Control.Applicative (liftA2)
#endif
import Control.Applicative (Alternative (empty, (<|>)))
import Data.Tuple (swap)

-- | An `Applicative`-based effect for executing computations in parallel.
data Parallel f a where
    -- | Executes two actions in parallel and blocks until both are complete.
    -- Finally, aggregates the execution results based on the specified function.
    LiftP2
        :: (a -> b -> c)
        -- ^ A function that aggregates the two execution results.
        -> f a
        -- ^ The first action to be executed in parallel.
        -> f b
        -- ^ The second action to be executed in parallel.
        -> Parallel f c

-- | An effect that blocks a computation indefinitely.
data Halt (a :: Type) where
    -- | Blocks a computation indefinitely.
    Halt :: Halt a

{- |
An effect that adopts the result of the computation that finishes first among
two computations and cancels the other.
-}
data Race f (a :: Type) where
    -- | Adopts the result of the computation that finishes first among two
    --   computations and cancels the other.
    Race :: f a -> f a -> Race f a

makeEffect [''Halt] [''Parallel, ''Race]

{- |
A wrapper that allows using the `Parallel` effect in the form of `Applicative` /
 `Alternative` instances.
-}
newtype Concurrently f a = Concurrently {runConcurrently :: f a}
    deriving (Functor)

instance (Parallel <<: f, Applicative f) => Applicative (Concurrently f) where
    pure = Concurrently . pure
    {-# INLINE pure #-}

    liftA2 f (Concurrently a) (Concurrently b) = Concurrently $ liftP2 f a b
    {-# INLINE liftA2 #-}

instance (Race <<: f, Halt <: f, Parallel <<: f, Applicative f) => Alternative (Concurrently f) where
    empty = Concurrently halt
    {-# INLINE empty #-}

    (Concurrently a) <|> (Concurrently b) = Concurrently $ race a b
    {-# INLINE (<|>) #-}

{- |
Executes three actions in parallel and blocks until all are complete.
Finally, aggregates the execution results based on the specified function.
-}
liftP3
    :: (Parallel <<: f, Applicative f)
    => (a -> b -> c -> d)
    -- ^ A function that aggregates the three execution results.
    -> f a
    -- ^ The first action to be executed in parallel.
    -> f b
    -- ^ The second action to be executed in parallel.
    -> f c
    -- ^ The third action to be executed in parallel.
    -> f d
liftP3 f a b = liftP2 ($) (liftP2 f a b)
{-# INLINE liftP3 #-}

-- | An effect that realizes polling and cancellation of actions running in parallel.
data Poll f a where
    -- | Performs polling on an action running in parallel in the form of a fold.
    --
    -- First, the parallel execution of two actions begins.
    --
    -- When the execution of the first action completes, polling on the second
    -- action is performed at that point, and the result is passed to the
    -- folding function. If the function returns `Left`, the folding terminates
    -- and it becomes the final result. If the second action is not yet
    -- complete, it is canceled. If the function returns `Right`, the folding
    -- continues, and the same process repeats.
    Poldl
        :: (a -> Maybe b -> f (Either r a))
        -- ^ A function for folding.
        -> f a
        -- ^ The first action to be executed in parallel.
        -> f b
        -- ^ The second action to be executed in parallel; the target of polling.
        -> Poll f r

makeEffectH [''Poll]

-- | Executes two actions in parallel. If the first action completes before the second, the second action is canceled.
cancels
    :: (Poll <<: f, Applicative f)
    => f a
    -- ^ The action that controls the cancellation.
    -> f b
    -- ^ The action to be canceled.
    -> f (a, Maybe b)
cancels = poldl $ curry $ pure . Left
{-# INLINE cancels #-}

-- | Executes two actions in parallel. If the second action completes before the first, the first action is canceled.
cancelBy
    :: (Poll <<: f, Applicative f)
    => f a
    -- ^ The action to be canceled.
    -> f b
    -- ^ The action that controls the cancellation.
    -> f (Maybe a, b)
cancelBy = flip $ poldl $ curry $ pure . Left . swap
{-# INLINE cancelBy #-}

-- | An effect for parallel computations based on a `Traversable` container @t@.
data For (t :: Type -> Type) f a where
    -- | Executes in parallel the actions stored within a `Traversable` container @t@.
    For :: t (f a) -> For t f (t a)

makeEffectH_ [''For]
makeHFunctor' ''For \(t :< _) -> [t|Functor $t|]

-- | Converts the `Traversable` container-based parallel computation effect t`For` into the `Applicative`-based parallel computation effect `Parallel`.
forToParallel :: (Parallel <<: f, Traversable t, Applicative f) => For t f ~> f
forToParallel (For iters) = runConcurrently $ traverse Concurrently iters
{-# INLINE forToParallel #-}
