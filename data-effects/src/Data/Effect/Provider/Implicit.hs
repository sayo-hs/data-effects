{-# LANGUAGE AllowAmbiguousTypes #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
module Data.Effect.Provider.Implicit where

data ImplicitProvider i hdls (f :: Type -> Type) (a :: Type) where
    WithImplicit :: i -> (hdls f -> f a) -> ImplicitProvider i hdls f a
makeEffectH_ [''ImplicitProvider]

infixl 2 .!

(.!)
    :: forall i hdls f a
     . (ImplicitProvider i hdls <<: f)
    => i
    -> (hdls f -> f a)
    -> f a
i .! f = withImplicit i f
{-# INLINE (.!) #-}
