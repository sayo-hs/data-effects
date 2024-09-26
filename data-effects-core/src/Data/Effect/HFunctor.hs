-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

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
