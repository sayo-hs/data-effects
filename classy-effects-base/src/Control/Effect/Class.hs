{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Comp.Multi.Derive (makeHFunctor)
import Data.Kind (Type)

-- | A kind of /signature/ (a datatype of higher-order effect).
type Signature = (Type -> Type) -> (Type -> Type)

-- | A kind of /instruction/ (a datatype of first-order effect).
type Instruction = Type -> Type

-- | A natural transformation.
type f ~> g = forall x. f x -> g x

{- | Lift an /instruction/ to a /signature/.

     Come from [heft-lang\/POPL2023\/haskell\/src\/Elab.hs]
    (https://github.com/heft-lang/POPL2023/blob/74afe1d5ce0b491cffe40cc5c73a2a5ee6a94d9c/haskell/src/Elab.hs#L9-L10).
-}

-- The code before modification is MIT licensed; (c) 2023 Casper Bach Poulsen and Cas van der Rest.
newtype LiftIns (ins :: Instruction) (f :: Type -> Type) a = LiftIns {unliftIns :: ins a}
    deriving stock (Functor, Foldable, Traversable)

makeHFunctor ''LiftIns

class SendSig (sig :: Signature) m where
    sendSig :: sig m a -> m a

class SendIns (ins :: Instruction) m where
    sendIns :: ins a -> m a

newtype SendVia m a = SendVia {runSendVia :: m a}
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadFix
        , MonadIO
        , MonadFail
        )
