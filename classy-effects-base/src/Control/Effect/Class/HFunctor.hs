module Control.Effect.Class.HFunctor (
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
