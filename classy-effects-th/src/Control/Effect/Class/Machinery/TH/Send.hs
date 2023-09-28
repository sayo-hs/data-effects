{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides @TemplateHaskell@ functions to derive an instance of the effect that handles
via 'SendIns'/'SendSig' type classes.
-}
module Control.Effect.Class.Machinery.TH.Send (
    module Control.Effect.Class.Machinery.TH.Send,
    EffectOrder (HigherOrder, FirstOrder),
)
where

import Control.Effect.Class (EffectDataHandler, EffectsVia, SendIns, SendSig)
import Control.Effect.Class.Machinery.TH.Send.Internal (effectMethodDec)
import Control.Exception (assert)
import Control.Monad (forM)
import Data.Effect.Class.TH.HFunctor.Internal (tyVarName)
import Data.Effect.Class.TH.Internal (
    EffectInfo,
    EffectOrder (FirstOrder, HigherOrder),
    MethodInterface (MethodInterface, methodName),
    effMethods,
    effMonad,
    effName,
    effParamVars,
    effectParamCxt,
    reifyEffectInfo,
    renameMethodToCon,
    superEffects,
    tyVarType,
    unkindTyVar,
 )
import Data.Maybe (maybeToList)
import Language.Haskell.TH (
    Dec (InstanceD),
    Name,
    Q,
    Type (ConT),
    appT,
    conT,
    varT,
 )

-- | Derive an instance of the effect that handles via 'SendIns'/'SendSig' type classes.
makeEffectSend ::
    -- | The class name of the effect.
    Name ->
    -- | The name and order of effect data type corresponding to the effect.
    Maybe (EffectOrder, Name) ->
    Q [Dec]
makeEffectSend effClsName effDataNameAndOrder = do
    info <- reifyEffectInfo effClsName
    sequence [deriveEffectSend info effDataNameAndOrder]

{- |
Derive an instance of the effect that handles via 'SendIns'/'SendSig' type classes, from
'EffectInfo'.
-}
deriveEffectSend ::
    -- | The reified information of the effect class.
    EffectInfo ->
    -- | The name and order of effect data type corresponding to the effect.
    Maybe (EffectOrder, Name) ->
    Q Dec
deriveEffectSend info effDataNameAndOrder = do
    let f = varT $ tyVarName $ effMonad info

        pvs = effParamVars info
        paramTypes = fmap (tyVarType . unkindTyVar) pvs

        carrier = [t|EffectsVia EffectDataHandler $f|]

        methods =
            [ (sig, renameMethodToCon methodName)
            | sig@MethodInterface{methodName} <- effMethods info
            ]

    sendCxt <-
        maybeToList
            <$> forM effDataNameAndOrder \(order, effDataName) ->
                assert (not $ null methods) do
                    let sendCls =
                            conT case order of
                                FirstOrder -> ''SendIns
                                HigherOrder -> ''SendSig

                        effData = foldl appT (conT effDataName) paramTypes

                    [t|$sendCls $effData $f|]

    let effParamCxt = effectParamCxt info

    superEffCxt <- forM (superEffects info) ((`appT` carrier) . pure)

    effDataC <- do
        let eff = pure $ ConT $ effName info
        [t|$(foldl appT eff paramTypes) $carrier|]

    decs <- mapM (uncurry $ effectMethodDec $ tyVarName <$> pvs) methods

    return $ InstanceD Nothing (sendCxt ++ superEffCxt ++ effParamCxt) effDataC (concat decs)
