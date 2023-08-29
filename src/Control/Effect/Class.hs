{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at https://mozilla.org/MPL/2.0/.           -}

module Control.Effect.Class where

import Data.Kind (Type)

type Signature = (Type -> Type) -> (Type -> Type)
type Instruction = Type -> Type

{- | Lift an /instruction/ (a datatype of first-order effect) to a /signature/
    (a datatype of higher-order effect).

     Come from [heft-lang\/POPL2023\/haskell\/src\/Elab.hs]
    (https://github.com/heft-lang/POPL2023/blob/74afe1d5ce0b491cffe40cc5c73a2a5ee6a94d9c/haskell/src/Elab.hs#L9-L10).
-}
newtype LiftIns (ins :: Instruction) (f :: Type -> Type) a = LiftIns {unliftIns :: ins a}
    deriving stock (Functor, Foldable, Traversable)

class Send (sig :: Signature) m where
    send :: sig m a -> m a

class SendF (ins :: Instruction) m where
    sendF :: ins a -> m a
