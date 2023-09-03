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
module Control.Effect.Class.Machinery.TH.Send where

import Control.Effect.Class (SendIns, SendSig, SendVia (SendVia), runSendVia, sendIns, sendSig)
import Control.Effect.Class.Machinery.HFunctor (hfmap)
import Control.Monad (forM, replicateM)
import Data.Effect.Class.TH (
    EffectInfo,
    EffectOrder (FirstOrder, HigherOrder),
    MethodInterface (MethodInterface, methodName),
    effMethods,
    effMonad,
    effName,
    effParamVars,
    effectParamCxt,
    methodOrder,
    methodParamTypes,
    reifyEffectInfo,
    renameMethodToCon,
    superEffects,
    tyVarType,
    unkindTyVar,
 )
import Data.Effect.Class.TH.HFunctor (tyVarName)
import Data.Maybe (maybeToList)
import Language.Haskell.TH (
    Dec (InstanceD),
    Inline (Inline),
    Name,
    Phases (AllPhases),
    Q,
    RuleMatch (FunLike),
    Type (ConT),
    appE,
    appT,
    clause,
    conE,
    conT,
    funD,
    newName,
    normalB,
    pragInlD,
    varE,
    varP,
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

    let pvs = effParamVars info
        paramTypes = fmap (tyVarType . unkindTyVar) pvs

        carrier = [t|SendVia $f|]

    sendCxt <-
        maybeToList
            <$> forM effDataNameAndOrder \(order, effDataName) -> do
                let sendCls =
                        conT case order of
                            FirstOrder -> ''SendIns
                            HigherOrder -> ''SendSig

                    effData = foldl appT (conT effDataName) paramTypes

                [t|$sendCls $effData $f|]

    let effParamCxt = effectParamCxt info
    superEffCxt <- forM (superEffects info) ((`appT` carrier) . pure)

    inst <- do
        let eff = pure $ ConT $ effName info
        [t|$(foldl appT eff paramTypes) $carrier|]

    decs <- do
        let methods =
                [ (sig, renameMethodToCon methodName)
                | sig@MethodInterface{methodName} <- effMethods info
                ]
        mapM (uncurry effectMethodDec) methods

    return $ InstanceD Nothing (sendCxt ++ superEffCxt ++ effParamCxt) inst (concat decs)

-- * Internal

{- |
Generate a method implementation of the effect that handles via 'SendIns'/'SendSig' type classes.
-}
effectMethodDec ::
    -- | The interface of the effect method.
    MethodInterface ->
    -- | The name of effect data constructor corresponding to the method.
    Name ->
    Q [Dec]
effectMethodDec MethodInterface{..} conName = do
    methodParams <- replicateM (length methodParamTypes) (newName "x")

    let ins = foldl appE (conE conName) (varE <$> methodParams)
        sendMethod = case methodOrder of
            FirstOrder -> [|sendIns|]
            HigherOrder -> [|sendSig . hfmap runSendVia|]
        body = [|SendVia $ $sendMethod $ins|]

    funDef <- funD methodName [clause (fmap varP methodParams) (normalB body) []]
    funInline <- pragInlD methodName Inline FunLike AllPhases

    return [funDef, funInline]
