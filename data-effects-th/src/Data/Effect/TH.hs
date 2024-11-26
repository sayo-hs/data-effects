{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.TH (
    module Data.Effect.TH,
    module Data.Default,
    module Data.Function,
    EffectOrder (..),
    EffectConf (..),
    keyedPerformerConf,
    normalPerformerConf,
    taggedPerformerConf,
    PerformerConf (..),
    performerName,
    doesGeneratePerformerSignature,
    performerDoc,
    performerArgDoc,
    performerConfs,
    deriveHFunctor,
    noGenerateNormalPerformer,
    noGenerateTaggedPerformer,
    noGenerateKeyedPerformer,
    noGeneratePerformerSignature,
) where

import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Writer.CPS (execWriterT, lift, tell)
import Data.Default (Default (def))
import Data.Effect (EffectOrder (FirstOrder, HigherOrder))
import Data.Effect.HFunctor.TH.Internal (deriveHFunctor)
import Data.Effect.TH.Internal (
    EffectConf (..),
    EffectGenerator,
    PerformerConf (..),
    doesGeneratePerformerSignature,
    genFOE,
    genHOE,
    keyedPerformerConf,
    noGenerateKeyedPerformer,
    noGenerateNormalPerformer,
    noGeneratePerformerSignature,
    noGenerateTaggedPerformer,
    normalPerformerConf,
    performerArgDoc,
    performerConfs,
    performerDoc,
    performerName,
    reifyEffect,
    taggedPerformerConf,
 )
import Data.Function ((&))
import Language.Haskell.TH (Dec, Name, Q, Type (TupleT))

makeEffectF :: Name -> Q [Dec]
makeEffectsF :: [Name] -> Q [Dec]
makeEffectF' :: EffectConf -> Name -> Q [Dec]
(makeEffectF, makeEffectsF, makeEffectF') = effectMakers genFOE

makeEffectH :: Name -> Q [Dec]
makeEffectsH :: [Name] -> Q [Dec]
makeEffectH' :: EffectConf -> Name -> Q [Dec]
(makeEffectH, makeEffectsH, makeEffectH') = effectMakers genHOEwithHFunctor

makeEffectH_ :: Name -> Q [Dec]
makeEffectsH_ :: [Name] -> Q [Dec]
makeEffectH_' :: EffectConf -> Name -> Q [Dec]
(makeEffectH_, makeEffectsH_, makeEffectH_') = effectMakers genHOE

effectMakers
    :: EffectGenerator
    -> ( Name -> Q [Dec]
       , [Name] -> Q [Dec]
       , EffectConf -> Name -> Q [Dec]
       )
effectMakers gen =
    ( execWriterT . gen' def
    , execWriterT . mapM (gen' def)
    , \conf -> execWriterT . gen' conf
    )
  where
    gen' conf e = do
        (info, dataInfo, eInfo) <- reifyEffect e & lift
        runReaderT gen (conf, e, info, dataInfo, eInfo)

genHOEwithHFunctor :: EffectGenerator
genHOEwithHFunctor = do
    genHOE
    (_, _, _, dataInfo, _) <- ask
    deriveHFunctor (const $ pure $ TupleT 0) dataInfo & lift & lift >>= tell
