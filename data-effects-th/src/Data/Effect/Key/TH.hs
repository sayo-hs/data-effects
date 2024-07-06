{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.Key.TH where

import Control.Effect.Key (SendInsBy, SendSigBy)
import Control.Lens ((%~), (<&>), _Just, _head)
import Control.Monad (forM_)
import Control.Monad.Writer (execWriterT, tell)
import Data.Char (toLower)
import Data.Default (def)
import Data.Effect.Key (type (##>), type (#>))
import Data.Effect.TH (makeEffect')
import Data.Effect.TH.Internal (
    DataInfo,
    EffClsInfo (EffClsInfo),
    EffConInfo (EffConInfo),
    EffectClassConf (EffectClassConf),
    EffectConf (EffectConf, _keyedSenderGenConf),
    EffectOrder (FirstOrder, HigherOrder),
    MakeEffectConf,
    SenderFunctionConf (SenderFunctionConf),
    alterEffectConf,
    ecEffs,
    ecName,
    ecParamVars,
    effName,
    genSenderArmor,
    normalSenderGenConf,
    senderFnName,
    tyVarName,
    _confByEffect,
    _keyedSenderGenConf,
    _senderFnName,
 )
import Data.Function ((&))
import Data.List.Extra (stripSuffix)
import Data.Text qualified as T
import Formatting (sformat, string, (%))
import Language.Haskell.TH (
    Body (NormalB),
    Clause (Clause),
    Dec (DataD, TySynD),
    Exp (AppTypeE, VarE),
    Info,
    Name,
    Q,
    TyVarBndr (PlainTV),
    Type (AppT, ConT, InfixT, VarT),
    mkName,
    nameBase,
 )

makeKeyedEffect :: [Name] -> [Name] -> Q [Dec]
makeKeyedEffect =
    makeEffect'
        (def & changeNormalSenderFnNameFormat)
        genEffectKey
{-# INLINE makeKeyedEffect #-}

changeNormalSenderFnNameFormat :: MakeEffectConf -> MakeEffectConf
changeNormalSenderFnNameFormat =
    alterEffectConf $ normalSenderGenConf . _Just . senderFnName %~ (++ "'_")
{-# INLINE changeNormalSenderFnNameFormat #-}

genEffectKey :: EffectOrder -> Info -> DataInfo -> EffClsInfo -> EffectClassConf -> Q [Dec]
genEffectKey order _ _ EffClsInfo{..} EffectClassConf{..} = execWriterT do
    let keyedOp = case order of
            FirstOrder -> ''(#>)
            HigherOrder -> ''(##>)

        pvs = tyVarName <$> ecParamVars

    ecNamePlain <-
        removeLastApostrophe (nameBase ecName)
            & maybe
                ( fail . T.unpack $
                    sformat
                        ("No last apostrophe on the effect class ‘" % string % "’.")
                        (nameBase ecName)
                )
                pure

    let keyDataName = mkName $ ecNamePlain ++ "Key"
        key = ConT keyDataName

    tell [DataD [] keyDataName [] Nothing [] []]

    tell
        [ TySynD
            (mkName ecNamePlain)
            (pvs <&> (`PlainTV` ()))
            (InfixT key keyedOp (foldl AppT (ConT ecName) (map VarT pvs)))
        ]

    forM_ ecEffs \con@EffConInfo{..} -> do
        let EffectConf{..} = _confByEffect effName
        forM_ _keyedSenderGenConf \conf@SenderFunctionConf{..} -> do
            let sendCxt effDataType carrier = case order of
                    FirstOrder -> ConT ''SendInsBy `AppT` key `AppT` effDataType `AppT` carrier
                    HigherOrder -> ConT ''SendSigBy `AppT` key `AppT` effDataType `AppT` carrier

            genSenderArmor sendCxt id conf{_senderFnName = nameBase effName & _head %~ toLower} con \_f ->
                pure $ Clause [] (NormalB $ VarE (mkName _senderFnName) `AppTypeE` key) []

removeLastApostrophe :: String -> Maybe String
removeLastApostrophe = stripSuffix "'"
