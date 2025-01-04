-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect.Interpret where

import Control.Effect (Eff (..), Free (liftFree, runFree), hoist, retract, type (~>))
import Data.Effect (Emb, getEmb)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.OpenUnion (KnownOrder, extract, (!+))

interpret
    :: forall e es ff a c
     . (Free c ff, KnownOrder e)
    => (e (Eff ff es) ~> Eff ff es)
    -> Eff ff (e ': es) a
    -> Eff ff es a
interpret i = loop
  where
    loop :: Eff ff (e ': es) ~> Eff ff es
    loop = Eff . runFree ((unEff . i !+ liftFree) . hfmap loop) . unEff
{-# INLINE interpret #-}

runEff :: (Free c ff, c f) => Eff ff '[Emb f] a -> f a
runEff = runFree (getEmb . extract) . unEff
{-# INLINE runEff #-}
