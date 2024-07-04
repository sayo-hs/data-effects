-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module re-exports the `HFunctor` type class and related definitions from the `compdata`
package, which are required for the Heftia effect handler system. For more details, please refer to
 [CEP-03](https://github.com/sayo-hs/data-effects/blob/master/CEPs/CEP-03.md) and
the [`compdata` documentation](https://hackage.haskell.org/package/compdata-0.13.0/docs/Data-Comp-Multi-HFunctor.html).
-}

module Data.Effect.HFunctor (
    HFunctor (..),
    makeHFunctor,
    RemA (..),
    DistAnn (..),
    (:&:) (..),
    (:=:),
    (:<:),
    Subsume (..),
    Elem,
    (:+:) (..),
    caseH,
    inj,
    proj,
    spl,
) where

import Data.Comp.Multi.Derive (makeHFunctor)
import Data.Comp.Multi.HFunctor (HFunctor (hfmap))
import Data.Comp.Multi.Ops (
    DistAnn (injectA, projectA),
    Elem,
    RemA (remA),
    Subsume (..),
    caseH,
    inj,
    proj,
    spl,
    type (:&:) ((:&:)),
    type (:+:) (Inl, Inr),
    type (:<:),
    type (:=:),
 )
