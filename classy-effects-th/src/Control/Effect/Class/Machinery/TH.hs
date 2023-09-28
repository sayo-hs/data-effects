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
module Control.Effect.Class.Machinery.TH where

import Control.Effect.Class.Machinery.TH.Internal (
    generateEffect,
    generateEffectWith,
    generateOrderUnifiedEffDataTySyn,
    generateOrderUnifiedEffectClass,
    unifyEffTypeParams,
 )
import Control.Effect.Class.Machinery.TH.Send.Internal (deriveEffectSend)
import Control.Monad (unless, (<=<))
import Control.Monad.Writer (execWriterT, lift, tell)
import Data.Effect.Class.TH.Internal (
    EffectOrder (FirstOrder, HigherOrder),
    defaultEffectDataNamer,
    effMethods,
    reifyEffectInfo,
 )
import Data.Function ((&))
import Language.Haskell.TH (Dec, Name, Q, mkName, nameBase)

{- |
In addition to 'makeEffectF' and 'makeEffectH',
generate the order-unified empty effect class:

    @class (FoobarF ... f, FoobarH ... f) => Foobar ... f@

, and generate the order-unified effect data type synonym:

    @type Foobar ... = FoobarS ... :+: LiftIns (FoobarI ...)@
-}
makeEffect ::
    -- | A name of order-unified empty effect class generated newly
    String ->
    -- | The name of first-order effect class
    Name ->
    -- | The name of higher-order effect class
    Name ->
    Q [Dec]
makeEffect clsU clsF clsH = do
    makeEffectWith
        clsU
        (clsU <> "D")
        clsF
        (defaultEffectDataNamer FirstOrder $ nameBase clsF)
        clsH
        (defaultEffectDataNamer HigherOrder $ nameBase clsH)

{- |
Generate an /instruction/ data type and type and pattern synonyms for abbreviating
'Control.Effect.Class.LiftIns'.
-}
makeEffectF :: Name -> Q [Dec]
makeEffectF = generateEffect FirstOrder <=< reifyEffectInfo

-- | Generate a /signature/ data type and a 'Data.Comp.Multi.HFunctor.HFunctor' instance.
makeEffectH :: Name -> Q [Dec]
makeEffectH = generateEffect HigherOrder <=< reifyEffectInfo

{- |
In addition to 'makeEffectF' and 'makeEffectH',
generate the order-unified empty effect class:

    @class (FoobarF ... f, FoobarH ... f) => Foobar ... f@

, and generate the order-unified effect data type synonym:

    @type Foobar ... = FoobarS ... :+: LiftIns (FoobarI ...)@
-}
makeEffectWith ::
    -- | A name of order-unified empty effect class generated newly
    String ->
    -- | A name of type synonym of order-unified effect data type generated newly
    String ->
    -- | The name of first-order effect class
    Name ->
    -- | The name of instruction data type corresponding to the first-order effect class
    String ->
    -- | The name of higher-order effect class
    Name ->
    -- | The name of signature data type corresponding to the higher-order effect class
    String ->
    Q [Dec]
makeEffectWith clsU dataU clsF dataI clsH dataS =
    execWriterT do
        let dataI' = mkName dataI
            dataS' = mkName dataS

        infoF <- reifyEffectInfo clsF & lift
        infoH <- reifyEffectInfo clsH & lift

        pvs <- unifyEffTypeParams infoF infoH & lift

        generateEffectWith FirstOrder dataI' infoF & lift >>= tell
        generateEffectWith HigherOrder dataS' infoH & lift >>= tell

        generateOrderUnifiedEffectClass infoF infoH pvs (mkName clsU) & lift >>= tell

        [generateOrderUnifiedEffDataTySyn dataI' dataS' pvs (mkName dataU)]
            & lift . sequence
            >>= tell

        pure ()

{- |
Generate an /instruction/ data type and type and pattern synonyms for abbreviating
'Control.Effect.Class.LiftIns'.
-}
makeEffectFWith :: String -> Name -> Q [Dec]
makeEffectFWith dataI = generateEffectWith FirstOrder (mkName dataI) <=< reifyEffectInfo

-- | Generate a /signature/ data type and a 'Data.Comp.Multi.HFunctor.HFunctor' instance.
makeEffectHWith :: String -> Name -> Q [Dec]
makeEffectHWith dataS = generateEffectWith HigherOrder (mkName dataS) <=< reifyEffectInfo

{- |
Derive an instance of the effect, with no methods, that handles via 'Control.Effect.Class.SendIns'/
'Control.Effect.Class.SendSig' instances.
-}
makeEmptyEffect :: Name -> Q [Dec]
makeEmptyEffect effClsName = do
    info <- reifyEffectInfo effClsName

    unless (null $ effMethods info) $
        fail ("The effect class \'" <> nameBase effClsName <> "\' is not empty.")

    sequence [deriveEffectSend info Nothing]

{- |
Generate the order-unified empty effect class:

    @class (FoobarF ... f, FoobarH ... f) => Foobar ... f@

, and derive an instance of the effect that handles via 'Control.Effect.Class.SendIns'/
'Control.Effect.Class.SendSig' instances.
-}
makeOrderUnifiedEffectClass :: Name -> Name -> String -> Q [Dec]
makeOrderUnifiedEffectClass clsF clsH clsU = do
    infoF <- reifyEffectInfo clsF
    infoH <- reifyEffectInfo clsH
    pvs <- unifyEffTypeParams infoF infoH
    generateOrderUnifiedEffectClass infoF infoH pvs (mkName clsU)
