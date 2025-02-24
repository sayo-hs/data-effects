-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

This module re-exports the `HFunctor` type class and related definitions from the `compdata`
package. For more details, please refer to the
    [`compdata` documentation](https://hackage.haskell.org/package/compdata-0.13.0/docs/Data-Comp-Multi-HFunctor.html).
-}
module Data.Effect.HFunctor (
    HFunctor (..),
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
