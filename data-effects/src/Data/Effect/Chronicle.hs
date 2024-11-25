{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.Chronicle where

import Data.Functor (($>))
import Data.These (These (That, These, This))

data ChronicleF c :: Effect where
    Dictate :: c -> ChronicleF c f ()
    Confess :: c -> ChronicleF c f a

data ChronicleH c :: Effect where
    Memento :: f a -> ChronicleH c f (Either c a)
    Absolve :: a -> f a -> ChronicleH c f a
    Condemn :: f a -> ChronicleH c f a

makeEffectF ''ChronicleF
makeEffectH ''ChronicleH

chronicle :: (ChronicleF c <! f, Applicative f) => These c a -> f a
chronicle = \case
    This c -> confess c
    That x -> pure x
    These c x -> dictate c $> x
