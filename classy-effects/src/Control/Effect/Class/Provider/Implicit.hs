{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Class.Provider.Implicit where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Data.Effect.Class.TH (makeSignature)

class ImplicitProvider c i e (f :: Type -> Type) | i f -> c e where
    withImplicit :: i -> (forall g. (c g, e g) => (f ~> g) -> g a) -> f a

makeSignature ''ImplicitProvider
makeEffectInfoTypeInstances ''ImplicitProvider (Just (HigherOrder, ''ImplicitProviderS))
makeEffectSend ''ImplicitProvider (Just (HigherOrder, ''ImplicitProviderS))

instance HFunctor (ImplicitProviderS c i e) where
    hfmap phi (WithImplicit i f) = WithImplicit i \emb -> f $ emb . phi

type MonadImplicitProvider = ImplicitProvider Monad
type ApplicativeImplicitProvider = ImplicitProvider Applicative

(.!) :: ImplicitProvider c i e f => i -> (forall g. (c g, e g) => g a) -> f a
i .! m = withImplicit i \_ -> m
{-# INLINE (.!) #-}

(..!) :: ImplicitProvider c i e f => i -> (forall g. (c g, e g) => (f ~> g) -> g a) -> f a
i ..! f = withImplicit i f
{-# INLINE (..!) #-}
