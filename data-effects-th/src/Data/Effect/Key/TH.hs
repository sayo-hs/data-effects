{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.Key.TH where

import Control.Effect.Key (PerformBy)
import Control.Lens ((%~), (<&>), _1, _Just, _head)
import Control.Monad (forM_)
import Control.Monad.Reader (ask, local)
import Control.Monad.Trans (lift)
import Control.Monad.Writer.CPS (tell)
import Data.Char (toLower)
import Data.Effect.Key (type (#>))
import Data.Effect.TH (
    EffectConf (..),
    SenderFunctionConf (
        SenderFunctionConf,
        _doesGenerateSenderFnSignature,
        _senderFnArgDoc,
        _senderFnDoc,
        _senderFnName
    ),
    effectMakers,
    genHOEwithHFunctor,
    normalSenderGenConf,
    senderFnName,
    (&),
 )
import Data.Effect.TH.Internal (
    EffectGenerator,
    EffectInfo (..),
    OpConf (
        OpConf,
        _keyedSenderGenConf,
        _normalSenderGenConf,
        _taggedSenderGenConf
    ),
    OpInfo (
        OpInfo,
        opCarrier,
        opCxt,
        opDataType,
        opName,
        opOrder,
        opParamTypes,
        opResultType,
        opTyVars
    ),
    alterOpConf,
    genFOE,
    genHOE,
    genSenderArmor,
    tyVarName,
 )
import Data.List.Extra (stripSuffix)
import Data.Text qualified as T
import Formatting (sformat, string, (%))
import Language.Haskell.TH (
    Body (NormalB),
    Clause (Clause),
    Dec (DataD, TySynD),
    Exp (AppTypeE, VarE),
    Name,
    Q,
    TyVarBndr (PlainTV),
    Type (AppT, ConT, InfixT, VarT),
    mkName,
    nameBase,
 )
import Language.Haskell.TH.Datatype.TyVarBndr (pattern BndrReq)

makeKeyedEffectF :: Name -> Q [Dec]
makeKeyedEffectsF :: [Name] -> Q [Dec]
makeKeyedEffectF' :: EffectConf -> Name -> Q [Dec]
(makeKeyedEffectF, makeKeyedEffectsF, makeKeyedEffectF') = effectMakers $ genKeyedEffect genFOE

makeKeyedEffectH :: Name -> Q [Dec]
makeKeyedEffectsH :: [Name] -> Q [Dec]
makeKeyedEffectH' :: EffectConf -> Name -> Q [Dec]
(makeKeyedEffectH, makeKeyedEffectsH, makeKeyedEffectH') = effectMakers $ genKeyedEffect genHOEwithHFunctor

makeKeyedEffectH_ :: Name -> Q [Dec]
makeKeyedEffectsH_ :: [Name] -> Q [Dec]
makeKeyedEffectH_' :: EffectConf -> Name -> Q [Dec]
(makeKeyedEffectH_, makeKeyedEffectsH_, makeKeyedEffectH_') = effectMakers $ genKeyedEffect genHOE

genKeyedEffect :: EffectGenerator -> EffectGenerator
genKeyedEffect gen = do
    local (_1 %~ changeNormalSenderFnNameFormat) gen
    genEffectKey

changeNormalSenderFnNameFormat :: EffectConf -> EffectConf
changeNormalSenderFnNameFormat =
    alterOpConf $ normalSenderGenConf . _Just . senderFnName %~ (++ "'_")
{-# INLINE changeNormalSenderFnNameFormat #-}

genEffectKey :: EffectGenerator
genEffectKey = do
    (EffectConf{..}, _, _, _, EffectInfo{..}) <- ask

    let keyedOp = ''(#>)

        pvs = tyVarName <$> eParamVars

    ecNamePlain <-
        removeLastApostrophe (nameBase eName)
            & maybe
                ( fail . T.unpack $
                    sformat
                        ("No last apostrophe on the effect class ‘" % string % "’.")
                        (nameBase eName)
                )
                pure

    let keyDataName = mkName $ ecNamePlain ++ "Key"
        key = ConT keyDataName

    tell [DataD [] keyDataName [] Nothing [] []]

    tell
        [ TySynD
            (mkName ecNamePlain)
            (pvs <&> (`PlainTV` BndrReq))
            (InfixT key keyedOp (foldl AppT (ConT eName) (map VarT pvs)))
        ]

    forM_ eOps \op@OpInfo{..} -> do
        let OpConf{..} = opConf opName
        forM_ _keyedSenderGenConf \conf@SenderFunctionConf{..} -> do
            let sendCxt effDataType carrier =
                    ConT ''PerformBy `AppT` key `AppT` effDataType `AppT` carrier

            lift $ genSenderArmor sendCxt id op conf{_senderFnName = nameBase opName & _head %~ toLower} \_f ->
                pure $ Clause [] (NormalB $ VarE (mkName _senderFnName) `AppTypeE` key) []

removeLastApostrophe :: String -> Maybe String
removeLastApostrophe = stripSuffix "'"
