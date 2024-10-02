{-# LANGUAGE PatternSynonyms #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.Key where

import Data.Comp.Multi.HFunctor (HFunctor)
import Data.Effect (InsClass, IsHFunctor, SigClass)

-- | Keyed /instruction class/.
newtype Key key (ins :: InsClass) a = Key {unKey :: ins a}
    deriving stock (Functor, Foldable, Traversable)

-- | Keyed /instruction class/.
type (#>) = Key

infixr 7 #>

-- | Keyed /instruction class/.
pattern K :: forall key ins a. ins a -> Key key ins a
pattern K e = Key e

{-# COMPLETE K #-}

-- | Keyed /signature class/.
newtype KeyH key (sig :: SigClass) f a = KeyH {unKeyH :: sig f a}
    deriving stock (Functor, Foldable, Traversable)
    deriving newtype (HFunctor)

-- | Keyed /signature class/.
type (##>) = KeyH

infixr 7 ##>

-- | Keyed /signature class/.
pattern KH :: forall key sig f a. sig f a -> KeyH key sig f a
pattern KH e = KeyH e

{-# COMPLETE KH #-}

type instance IsHFunctor (KeyH key sig) = IsHFunctor sig
