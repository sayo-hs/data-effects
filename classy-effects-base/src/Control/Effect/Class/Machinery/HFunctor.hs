-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Machinery.HFunctor (
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
