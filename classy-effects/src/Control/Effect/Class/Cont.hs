-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Cont where

import Data.Effect.Class.TH (makeSignature)

class Monad m => MonadCont m where
    callCC :: (forall r. (a -> m r) -> m a) -> m a

makeSignature ''MonadCont

{- is this impossible?
instance HFunctor (MonadContS) where
    hfmap f (CallCC g) =
-}
