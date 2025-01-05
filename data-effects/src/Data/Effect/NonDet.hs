{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   : (c) 2024-2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects that realize non-deterministic computations.
-}
module Data.Effect.NonDet where

import Control.Applicative (Alternative, (<|>))
import Control.Applicative qualified as A
import Control.Exception (Exception, SomeException)
import Data.Bool (bool)
import Data.Effect (Emb)
import Data.Effect.Unlift (UnliftIO)
import UnliftIO (throwIO, try)

-- | An effect that eliminates a branch by causing the current branch context of a non-deterministic computation to fail.
data Empty :: Effect where
    -- | Eliminates a branch by causing the current branch context of a non-deterministic computation to fail.
    Empty :: Empty f a

makeEffectF ''Empty

-- | An effect that splits the computation into two branches.
data Choose :: Effect where
    -- | Splits the computation into two branches.
    -- As a result of executing @choose@, the world branches into one where `False` is returned and one where `True` is returned.
    Choose :: Choose f Bool

makeEffectF ''Choose

{- |
An effect that executes two branches as scopes.
A higher-order version of the t`Choose` effect.
-}
data ChooseH :: Effect where
    -- | Executes the given two scopes as branches.
    -- Even if one fails due to the `empty` operation, the whole does not fail as long as the other does not fail.
    ChooseH :: f a -> f a -> ChooseH f a

makeEffectH ''ChooseH

-- fixme: orphan
instance
    (Empty :> es, ChooseH :> es, Applicative (Eff ff es), Free c ff)
    => Alternative (Eff ff es)
    where
    empty = empty
    a <|> b = chooseH a b
    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

{- | t'ChooseH' effect elaborator.

    Convert a higher-order effect of the form

        @chooseH :: m a -> m a -> m a@

    into a first-order effect of the form:

        @choose :: m Bool@
-}
runChooseH
    :: (Choose :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (ChooseH ': es) a
    -> Eff ff es a
runChooseH = interpret \(ChooseH a b) -> branch a b

-- | Faster than `<|>`.
branch :: (Choose :> es, Monad (Eff ff es), Free c ff) => Eff ff es a -> Eff ff es a -> Eff ff es a
branch a b = do
    world <- choose
    bool a b world
{-# INLINE branch #-}

infixl 3 `branch`

-- | Selects one element from the list nondeterministically, branching the control as many times as the number of elements.
choice :: (Choose :> es, Empty :> es, Monad (Eff ff es), Free c ff) => [a] -> Eff ff es a
choice = \case
    [] -> empty
    x : xs -> pure x `branch` choice xs

-- | Selects one element from the list nondeterministically, branching the control as many times as the number of elements. Uses t'ChooseH'.
choiceH :: (ChooseH :> es, Empty :> es, Monad (Eff ff es), Free c ff) => [a] -> Eff ff es a
choiceH = \case
    [] -> empty
    x : xs -> pure x <|> choiceH xs

{- |
Interprets the [NonDet]("Data.Effect.NonDet") effects using IO-level exceptions.

When 'empty' occurs, an v'EmptyException' is thrown, and unless all branches from
 'chooseH' fail due to IO-level exceptions, only the leftmost result is returned
 as the final result.
-}
runNonDetIO
    :: (UnliftIO :> es, Emb IO `In` es, Free c ff, forall f. Monad (ff f))
    => Eff ff (ChooseH ': Empty ': es) a
    -> Eff ff es (Either SomeException a)
runNonDetIO m = try do
    m
        & interpret
            ( \(ChooseH a b) ->
                try a >>= \case
                    Right x -> pure x
                    Left (_ :: SomeException) -> b
            )
        & interpret (\Empty -> throwIO EmptyException)

-- | Exception thrown when 'empty' occurs in 'runNonDetIO'.
data EmptyException = EmptyException
    deriving stock (Show)
    deriving anyclass (Exception)
