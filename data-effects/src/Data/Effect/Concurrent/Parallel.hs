{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

module Data.Effect.Concurrent.Parallel where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Tuple (swap)

data Parallel f a where
    LiftP2 :: (a -> b -> c) -> f a -> f b -> Parallel f c

data Halt (a :: Type) where
    Halt :: Halt a

data Race f (a :: Type) where
    Race :: f a -> f a -> Race f a

makeEffect [''Halt] [''Parallel, ''Race]

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

liftP3 :: (Parallel <<: f, Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftP3 f a b = liftP2 ($) (liftP2 f a b)
{-# INLINE liftP3 #-}

data Poll f a where
    Poldl :: (a -> Maybe b -> f (Either r a)) -> f a -> f b -> Poll f r
makeEffectH [''Poll]

cancels :: (Poll <<: f, Applicative f) => f a -> f b -> f (a, Maybe b)
cancels = poldl $ curry $ pure . Left
{-# INLINE cancels #-}

cancelBy :: (Poll <<: f, Applicative f) => f a -> f b -> f (Maybe a, b)
cancelBy = flip $ poldl $ curry $ pure . Left . swap
{-# INLINE cancelBy #-}

data For (t :: Type -> Type) f a where
    For :: t (f a) -> For t f (t a)
makeEffectH_ [''For]
makeHFunctor' ''For \(t :< _) -> [t|Functor $t|]

forToParallel :: (Parallel <<: f, Traversable t, Applicative f) => For t f ~> f
forToParallel (For iters) = runConcurrently $ traverse Concurrently iters
{-# INLINE forToParallel #-}
