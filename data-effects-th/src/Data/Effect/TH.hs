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

import Control.Monad.Writer (execWriterT, lift, tell)
import Data.Default (def)
import Data.Effect.HFunctor.TH.Internal (deriveHFunctor)
import Data.Effect.TH.Internal (
    EffectOrder (FirstOrder, HigherOrder),
    MakeEffectConf (genSenderFnSignature),
    genLiftInsPatternSynonyms,
    genLiftInsTypeSynonym,
    makeSenderAll,
    reifyEffCls,
 )
import Data.Function ((&))
import Data.List (singleton)
import Language.Haskell.TH (Dec, Name, Q)

makeEffectF' :: MakeEffectConf -> Name -> Q [Dec]
makeEffectF' conf name = do
    (_, _, effClsInfo) <- reifyEffCls FirstOrder name
    execWriterT do
        makeSenderAll conf effClsInfo & lift >>= tell
        genLiftInsTypeSynonym effClsInfo & singleton & tell
        genLiftInsPatternSynonyms effClsInfo & lift >>= tell
{-# INLINE makeEffectF' #-}

makeEffectF :: Name -> Q [Dec]
makeEffectF = makeEffectF' def
{-# INLINE makeEffectF #-}

makeEffectF_ :: Name -> Q [Dec]
makeEffectF_ = makeEffectF' def{genSenderFnSignature = False}
{-# INLINE makeEffectF_ #-}

makeEffectH' :: MakeEffectConf -> Name -> Q [Dec]
makeEffectH' conf name = do
    (_, dataInfo, effClsInfo) <- reifyEffCls HigherOrder name
    execWriterT do
        makeSenderAll conf effClsInfo & lift >>= tell
        deriveHFunctor dataInfo & lift >>= tell
{-# INLINE makeEffectH' #-}

makeEffectH :: Name -> Q [Dec]
makeEffectH = makeEffectH' def
{-# INLINE makeEffectH #-}

makeEffectH_ :: Name -> Q [Dec]
makeEffectH_ = makeEffectH' def{genSenderFnSignature = False}
{-# INLINE makeEffectH_ #-}

makeEffect' :: MakeEffectConf -> Name -> Name -> Q [Dec]
makeEffect' conf ins sig = execWriterT do
    makeEffectF' conf ins & lift >>= tell
    makeEffectH' conf sig & lift >>= tell

makeEffect :: Name -> Name -> Q [Dec]
makeEffect = makeEffect' def
{-# INLINE makeEffect #-}

makeEffect_ :: Name -> Name -> Q [Dec]
makeEffect_ = makeEffect' def{genSenderFnSignature = False}
{-# INLINE makeEffect_ #-}
