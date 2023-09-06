{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Machinery.TH.Send.Internal where

import Control.Effect.Class (
    EffectDataHandler,
    EffectsVia (EffectsVia),
    runEffectsVia,
    sendIns,
    sendSig,
 )
import Control.Effect.Class.Machinery.HFunctor (hfmap)
import Control.Monad (replicateM)
import Data.Effect.Class.TH.Internal (
    EffectOrder (FirstOrder, HigherOrder),
    MethodInterface (MethodInterface, methodName),
    methodOrder,
    methodParamTypes,
    methodReturnType,
 )
import Language.Haskell.TH (
    Dec,
    Inline (Inline),
    Name,
    Phases (AllPhases),
    Q,
    RuleMatch (FunLike),
    appE,
    appTypeE,
    clause,
    conE,
    funD,
    newName,
    normalB,
    pragInlD,
    varE,
    varP,
    varT,
 )

{- |
Generate a method implementation of the effect that handles via 'Control.Effect.Class.SendIns'/
'Control.Effect.Class.SendSig' type classes.
-}
effectMethodDec ::
    -- | The type parameters of the effect.
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
