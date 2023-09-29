{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Machinery.TH.Send.Internal where

import Control.Effect.Class (
    EffectDataHandler,
    EffectDataToClass (EffectDataToClass),
    EffectsVia (EffectsVia),
    SendIns,
    SendSig,
    effectDataToClass,
    runEffectsVia,
    sendIns,
    sendSig,
 )
import Control.Effect.Class.Machinery.HFunctor (hfmap)
import Control.Exception (assert)
import Control.Monad (forM, replicateM)
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
    methodOrder,
    methodParamTypes,
    methodReturnType,
    renameMethodToCon,
    superEffects,
    tyVarType,
    unkindTyVar,
 )
import Data.Functor ((<&>))
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
    appTypeE,
    caseE,
    clause,
    conE,
    conP,
    conT,
    funD,
    match,
    newName,
    normalB,
    pragInlD,
    varE,
    varP,
    varT,
 )

{- |
Derive an instance of the effect that handles via 'SendIns'/'SendSig' type classes, from
'EffectInfo'.
-}
deriveEffectSend ::
    -- | The reified information of the effect class.
    EffectInfo ->
    -- | The name and order of effect data type corresponding to the effect class.
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

{- |
Generate a method implementation of the effect that handles via 'Control.Effect.Class.SendIns'/
'Control.Effect.Class.SendSig' type classes.
-}
effectMethodDec ::
    -- | The type parameters of the effect class.
    [Name] ->
    -- | The interface of the effect method.
    MethodInterface ->
    -- | The name of effect data constructor corresponding to the method.
    Name ->
    Q [Dec]
effectMethodDec effTyVars MethodInterface{..} conName = do
    methodParams <- replicateM (length methodParamTypes) (newName "x")

    let con = foldl appTypeE (conE conName) (varT <$> effTyVars)

        effData = foldl appE con (varE <$> methodParams)

        sendMethod = case methodOrder of
            FirstOrder -> [|sendIns|]
            HigherOrder -> [|sendSig . hfmap runEffectsVia|]
        body = [|EffectsVia @EffectDataHandler $ $sendMethod $effData|]

    funDef <- funD methodName [clause (fmap varP methodParams) (normalB body) []]
    funInline <- pragInlD methodName Inline FunLike AllPhases

    return [funDef, funInline]

-- | Derive instances of 'SendIns'/'SendSig' for `EffectDataToClass`.
deriveEffectRecv ::
    -- | The reified information of the effect class.
    EffectInfo ->
    -- | The order of the effect class.
    EffectOrder ->
    -- | The name of effect data type corresponding to the effect class.
    Name ->
    Q Dec
deriveEffectRecv info order effDataName = do
    let f = varT $ tyVarName $ effMonad info

        pvs = effParamVars info
        paramTypes = fmap (tyVarType . unkindTyVar) pvs

        carrier = [t|EffectDataToClass $f|]

        methods =
            [ (sig, renameMethodToCon methodName)
            | sig@MethodInterface{methodName} <- effMethods info
            ]

        sendMethod = case order of
            FirstOrder -> 'sendIns
            HigherOrder -> 'sendSig

    cxt <- do
        let eff = pure $ ConT $ effName info
        [t|$(foldl appT eff paramTypes) $f|]

    inst <-
        assert (not $ null methods) do
            let sendCls =
                    conT case order of
                        FirstOrder -> ''SendIns
                        HigherOrder -> ''SendSig

                effData = foldl appT (conT effDataName) paramTypes

            [t|$sendCls $effData $carrier|]

    eff <- newName "e"
    let eff' = varE eff
    let hfmapped = case order of
            FirstOrder -> eff'
            HigherOrder -> [|hfmap effectDataToClass $ $eff'|]
    let body =
            caseE hfmapped $
                methods <&> \(MethodInterface{..}, conName) -> do
                    effParams <- replicateM (length methodParamTypes) (newName "x")
                    let method = foldl appTypeE (varE methodName) (varT . tyVarName <$> pvs)
                    let methodCall = foldl appE method (varE <$> effParams)
                    match
                        (conP conName $ map varP effParams)
                        (normalB [|EffectDataToClass $methodCall|])
                        []
    funDef <- funD sendMethod [clause [varP eff] (normalB body) []]
    funInline <- pragInlD sendMethod Inline FunLike AllPhases

    return $ InstanceD Nothing [cxt] inst [funDef, funInline]
