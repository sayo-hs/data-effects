{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides the `Provider` effect, comes
from [@Effectful.Provider@](https://hackage.haskell.org/package/effectful-core-2.3.0.0/docs/Effectful-Provider.html)
in the @effectful@ package.
-}
module Control.Effect.Class.Provider where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Data.Effect.Class.TH (makeSignature)

class Provider c i ctx e (f :: Type -> Type) | i ctx f -> c e where
    provide :: i -> (forall g. (c g, e g) => (f ~> g) -> g a) -> f (ctx a)

makeSignature ''Provider
makeEffectInfoTypeInstances ''Provider (Just (HigherOrder, ''ProviderS))
makeEffectSend ''Provider (Just (HigherOrder, ''ProviderS))

instance HFunctor (ProviderS c i ctx e) where
    hfmap phi (Provide i f) = Provide i \emb -> f $ emb . phi

type MonadProvider = Provider Monad
type ApplicativeProvider = Provider Applicative

mprovide ::
    forall e i ctx f a.
    MonadProvider i ctx e f =>
    i ->
    (forall g. (Monad g, e g) => (f ~> g) -> g a) ->
    f (ctx a)
mprovide = provide
{-# INLINE mprovide #-}

aprovide ::
    forall e i ctx f a.
    ApplicativeProvider i ctx e f =>
    i ->
    (forall h. (Applicative h, e h) => (f ~> h) -> h a) ->
    f (ctx a)
aprovide = provide
{-# INLINE aprovide #-}
