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
    orderOf,
    MakeEffectConf (..),
    alterEffectClassConf,
    alterEffectConf,
    EffectClassConf (..),
    confByEffect,
    doesDeriveHFunctor,
    doesGenerateLiftFOEPatternSynonyms,
    doesGenerateLiftFOETypeSynonym,
    EffectConf (..),
    keyedSenderGenConf,
    normalSenderGenConf,
    taggedSenderGenConf,
    warnFirstOrderInHOE,
    SenderFunctionConf (..),
    senderFnName,
    doesGenerateSenderFnSignature,
    senderFnDoc,
    senderFnArgDoc,
    senderFnConfs,
    deriveHFunctor,
    noDeriveHFunctor,
    generateLiftFOETypeSynonym,
    noGenerateLiftFOETypeSynonym,
    generateLiftFOEPatternSynonyms,
    noGenerateLiftFOEPatternSynonyms,
    noGenerateNormalSenderFunction,
    noGenerateTaggedSenderFunction,
    noGenerateKeyedSenderFunction,
    suppressFirstOrderInHigherOrderEffectWarning,
    noGenerateSenderFunctionSignature,
) where

import Control.Monad (forM_, when)
import Control.Monad.Writer (execWriterT, lift, tell)
import Data.Default (Default (def))
import Data.Effect.HFunctor.TH.Internal (deriveHFunctor)
import Data.Effect.TH.Internal (
    DataInfo,
    EffClsInfo,
    EffectClassConf (
        EffectClassConf,
        _confByEffect,
        _doesDeriveHFunctor,
        _doesGenerateLiftFOEPatternSynonyms,
        _doesGenerateLiftFOETypeSynonym
    ),
    EffectConf (
        EffectConf,
        _keyedSenderGenConf,
        _normalSenderGenConf,
        _taggedSenderGenConf,
        _warnFirstOrderInHOE
    ),
    EffectOrder (FirstOrder, HigherOrder),
    MakeEffectConf (MakeEffectConf, unMakeEffectConf),
    SenderFunctionConf (
        _doesGenerateSenderFnSignature,
        _senderFnArgDoc,
        _senderFnDoc,
        _senderFnName
    ),
    alterEffectClassConf,
    alterEffectConf,
    confByEffect,
    doesDeriveHFunctor,
    doesGenerateLiftFOEPatternSynonyms,
    doesGenerateLiftFOETypeSynonym,
    doesGenerateSenderFnSignature,
    genLiftFOEPatternSynonyms,
    genLiftFOETypeSynonym,
    genSenders,
    generateLiftFOEPatternSynonyms,
    generateLiftFOETypeSynonym,
    keyedSenderGenConf,
    noDeriveHFunctor,
    noGenerateKeyedSenderFunction,
    noGenerateLiftFOEPatternSynonyms,
    noGenerateLiftFOETypeSynonym,
    noGenerateNormalSenderFunction,
    noGenerateSenderFunctionSignature,
    noGenerateTaggedSenderFunction,
    normalSenderGenConf,
    orderOf,
    reifyEffCls,
    senderFnArgDoc,
    senderFnConfs,
    senderFnDoc,
    senderFnName,
    suppressFirstOrderInHigherOrderEffectWarning,
    taggedSenderGenConf,
    unMakeEffectConf,
    warnFirstOrderInHOE,
 )
import Data.Function ((&))
import Data.List (singleton)
import Language.Haskell.TH (Dec, Info, Name, Q, Type (TupleT))

makeEffect'
    :: MakeEffectConf
    -> (EffectOrder -> Info -> DataInfo -> EffClsInfo -> EffectClassConf -> Q [Dec])
    -> [Name]
    -> [Name]
    -> Q [Dec]
makeEffect' (MakeEffectConf conf) extTemplate inss sigs = execWriterT do
    forM_ inss \ins -> do
        (info, dataInfo, effClsInfo) <- reifyEffCls FirstOrder ins & lift
        ecConf@EffectClassConf{..} <- conf effClsInfo & lift

        genSenders ecConf effClsInfo & lift >>= tell

        when _doesGenerateLiftFOETypeSynonym do
            genLiftFOETypeSynonym effClsInfo & singleton & tell

        when _doesGenerateLiftFOEPatternSynonyms do
            genLiftFOEPatternSynonyms effClsInfo & lift >>= tell

        extTemplate FirstOrder info dataInfo effClsInfo ecConf & lift >>= tell

    forM_ sigs \sig -> do
        (info, dataInfo, effClsInfo) <- reifyEffCls HigherOrder sig & lift
        ecConf@EffectClassConf{..} <- conf effClsInfo & lift

        genSenders ecConf effClsInfo & lift >>= tell

        when _doesDeriveHFunctor do
            deriveHFunctor (const $ pure $ TupleT 0) dataInfo & lift >>= tell

        extTemplate HigherOrder info dataInfo effClsInfo ecConf & lift >>= tell

noExtTemplate :: EffectOrder -> Info -> DataInfo -> EffClsInfo -> EffectClassConf -> Q [Dec]
noExtTemplate = mempty
{-# INLINE noExtTemplate #-}

makeEffect :: [Name] -> [Name] -> Q [Dec]
makeEffect = makeEffect' def noExtTemplate
{-# INLINE makeEffect #-}

makeEffectF :: [Name] -> Q [Dec]
makeEffectF inss = makeEffect inss []
{-# INLINE makeEffectF #-}

makeEffectH :: [Name] -> Q [Dec]
makeEffectH sigs = makeEffect [] sigs
{-# INLINE makeEffectH #-}

makeEffect_ :: [Name] -> [Name] -> Q [Dec]
makeEffect_ = makeEffect' (def & noDeriveHFunctor) noExtTemplate
{-# INLINE makeEffect_ #-}

makeEffectH_ :: [Name] -> Q [Dec]
makeEffectH_ sigs = makeEffect_ [] sigs
{-# INLINE makeEffectH_ #-}
