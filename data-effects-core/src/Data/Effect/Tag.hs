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
module Data.Effect.Tag where

import Data.Comp.Multi.HFunctor (HFunctor)
import Data.Effect (EffectF, EffectH)

-- | Tagged first-order effect.
newtype Tag (ins :: EffectF) tag a = Tag {unTag :: ins a}
    deriving stock (Functor, Foldable, Traversable)

-- | Tagged first-order effect.
type (#) = Tag

infixl 8 #

-- | Tagged first-order effect.
pattern T :: forall tag ins a. ins a -> Tag ins tag a
pattern T e = Tag e

{-# COMPLETE T #-}

-- | Tagged higher-order effect.
newtype TagH (sig :: EffectH) tag f a = TagH {unTagH :: sig f a}
    deriving stock (Functor, Foldable, Traversable)
    deriving newtype (HFunctor)

-- | Tagged higher-order effect.
type (##) = TagH

infixl 8 ##

-- | Tagged higher-order effect.
pattern TH :: forall tag sig f a. sig f a -> TagH sig tag f a
pattern TH e = TagH e

{-# COMPLETE TH #-}
