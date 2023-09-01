-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides @TemplateHaskell@ functions to generates automatically various data types and
instances that constitute the effect system supplied by the @classy-effects@ framework.
-}
module Control.Effect.Class.TH where

import Control.Effect.Class.TH.Send (deriveEffectSend)
import Control.Monad (forM_)
import Control.Monad.Writer (execWriterT, lift, tell)
import Data.Effect.Class.TH (
    EffectDataNamer,
    EffectOrder (FirstOrder, HigherOrder),
    MakeEmptyEffectData (MakeEffectDataEvenIfEmpty, NoMakeEmptyEffectData),
    defaultEffectDataNamer,
    generateEffectDataByEffInfo,
    getEffDataInfoOn,
    reifyEffectInfo,
    restrictEffectDataNamer,
 )
import Data.Effect.Class.TH.HFunctor (dataName, deriveHFunctor)
import Language.Haskell.TH (Dec, Name, Q)

{- |
Generate effect data types and derive 'Data.Comp.Multi.HFunctor.HFunctor' and
'Control.Effect.Class.Send'/'Control.Effect.Class.SendF' instances for the given effect class.
-}
makeEffect :: Name -> Q [Dec]
makeEffect = makeEffectWith defaultEffectDataNamer NoMakeEmptyEffectData

{- |
Generate only an /instruction/ data type (even if the one is empty) and a
'Control.Effect.Class.SendF' instance for the given effect class.
-}
makeEffectF :: Name -> Q [Dec]
makeEffectF =
    makeEffectWith
        (restrictEffectDataNamer FirstOrder defaultEffectDataNamer)
        MakeEffectDataEvenIfEmpty

{- |
Generate only a /signature/ data type (even if the one is empty) and derive
'Data.Comp.Multi.HFunctor.HFunctor' and a 'Control.Effect.Class.Send' instance for the given effect
class.
-}
makeEffectH :: Name -> Q [Dec]
makeEffectH =
    makeEffectWith
        (restrictEffectDataNamer HigherOrder defaultEffectDataNamer)
        MakeEffectDataEvenIfEmpty

{- |
Generate /instruction/ and /signature/ data types (even if the one is empty) and derive
'Data.Comp.Multi.HFunctor.HFunctor' and 'Control.Effect.Class.Send'/'Control.Effect.Class.SendF'
instances for the given effect class.
-}
makeEffectFH :: Name -> Q [Dec]
makeEffectFH = makeEffectWith defaultEffectDataNamer MakeEffectDataEvenIfEmpty

{- |
Generate effect data types with the given naming convention, and derive
'Data.Comp.Multi.HFunctor.HFunctor' and 'Control.Effect.Class.Send'/'Control.Effect.Class.SendF'
instances, for the given effect class.
-}
makeEffectWith ::
    -- | The naming convention of effect data types.
    EffectDataNamer ->
    -- | Whether to generate an effect data type even when the one is empty.
    MakeEmptyEffectData ->
    -- | The class name of the effect.
    Name ->
    Q [Dec]
makeEffectWith effDataNamer makeEmptyEffData effClsName = do
    info <- reifyEffectInfo effClsName

    execWriterT do
        (genResult, effDataDecs) <-
            lift $
                generateEffectDataByEffInfo
                    effDataNamer
                    makeEmptyEffData
                    info
        tell effDataDecs

        forM_ (getEffDataInfoOn HigherOrder genResult) \effDataHInfo ->
            tell =<< lift (deriveHFunctor effDataHInfo)

        tell =<< lift do
            sequence
                [ deriveEffectSend
                    info
                    (dataName <$> getEffDataInfoOn FirstOrder genResult)
                    (dataName <$> getEffDataInfoOn HigherOrder genResult)
                ]
