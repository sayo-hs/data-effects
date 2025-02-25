-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.HFunctor where

class HFunctor ff where
    hfmap :: (forall x. f x -> g x) -> ff f a -> ff g a
