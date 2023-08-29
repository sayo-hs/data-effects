{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at https://mozilla.org/MPL/2.0/.           -}

module Control.Effect.Class where

import Data.Kind (Type)

type Instruction = Type -> Type
type Signature = (Type -> Type) -> (Type -> Type)

{- | Lift an /instruction/ (a datatype of first-order effect) to a /signature/
    (a datatype of higher-order effect).

     Come from [heft-lang\/POPL2023\/haskell\/src\/Elab.hs]
    (https://github.com/heft-lang/POPL2023/blob/74afe1d5ce0b491cffe40cc5c73a2a5ee6a94d9c/haskell/src/Elab.hs#L9-L10).
-}
newtype LiftIns ins (f :: Type -> Type) (a :: Type) = LiftIns {unliftIns :: ins a}
    deriving stock (Functor, Foldable, Traversable)
