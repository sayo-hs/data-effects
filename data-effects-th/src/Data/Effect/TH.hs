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
    makeSenders,
    reifyEffCls,
 )
import Data.Function ((&))
import Data.List (singleton)
import Language.Haskell.TH (Dec, Name, Q)

makeEffect' :: [Name] -> [Name] -> MakeEffectConf -> Q [Dec]
makeEffect' inss sigs (MakeEffectConf conf) = execWriterT do
    forM_ inss \ins -> do
        (_, _, effClsInfo) <- reifyEffCls FirstOrder ins & lift
        ecConf@EffectClassConf{..} <- conf effClsInfo & lift

        makeSenders ecConf effClsInfo & lift >>= tell

        when _doesGenerateLiftInsTypeSynonym do
            genLiftInsTypeSynonym effClsInfo & singleton & tell

        when _doesGenerateLiftInsPatternSynonyms do
            genLiftInsPatternSynonyms effClsInfo & lift >>= tell

    forM_ sigs \sig -> do
        (_, dataInfo, effClsInfo) <- reifyEffCls HigherOrder sig & lift
        ecConf@EffectClassConf{..} <- conf effClsInfo & lift

        makeSenders ecConf effClsInfo & lift >>= tell

        when _doesDeriveHFunctor do
            deriveHFunctor dataInfo & lift >>= tell

makeEffect :: [Name] -> [Name] -> Q [Dec]
makeEffect inss sigs = makeEffect' inss sigs def
{-# INLINE makeEffect #-}

makeEffectF :: [Name] -> Q [Dec]
makeEffectF inss = makeEffect inss []
{-# INLINE makeEffectF #-}

makeEffectH :: [Name] -> Q [Dec]
makeEffectH sigs = makeEffect [] sigs
{-# INLINE makeEffectH #-}
