{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.TH where

import Control.Monad.Writer (execWriterT, forM_, lift, tell, when)
import Data.Default (def)
import Data.Effect.HFunctor.TH.Internal (deriveHFunctor)
import Data.Effect.TH.Internal (
    DataInfo,
    EffClsInfo,
    EffectClassConf (
        EffectClassConf,
        _confByEffect,
        _doesDeriveHFunctor,
        _doesGenerateLiftInsPatternSynonyms,
        _doesGenerateLiftInsTypeSynonym
    ),
    EffectOrder (FirstOrder, HigherOrder),
    MakeEffectConf (MakeEffectConf),
    genLiftInsPatternSynonyms,
    genLiftInsTypeSynonym,
    genSenders,
    reifyEffCls,
 )
import Data.Function ((&))
import Data.List (singleton)
import Language.Haskell.TH (Dec, Info, Name, Q, Type (TupleT))

makeEffect' ::
    MakeEffectConf ->
    (EffectOrder -> Info -> DataInfo -> EffClsInfo -> EffectClassConf -> Q [Dec]) ->
    [Name] ->
    [Name] ->
    Q [Dec]
makeEffect' (MakeEffectConf conf) extTemplate inss sigs = execWriterT do
    forM_ inss \ins -> do
        (info, dataInfo, effClsInfo) <- reifyEffCls FirstOrder ins & lift
        ecConf@EffectClassConf{..} <- conf effClsInfo & lift

        genSenders ecConf effClsInfo & lift >>= tell

        when _doesGenerateLiftInsTypeSynonym do
            genLiftInsTypeSynonym effClsInfo & singleton & tell

        when _doesGenerateLiftInsPatternSynonyms do
            genLiftInsPatternSynonyms effClsInfo & lift >>= tell

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
