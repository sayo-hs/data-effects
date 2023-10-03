{-# LANGUAGE TemplateHaskellQuotes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Machinery.TH.DepParams.Internal where

import Control.Effect.Class.Machinery.DepParams (
    DepParams,
    DepParamsOf,
    DepParamsOfC,
    DepParamsOfH,
    EffectClassIdentifierOf,
    EffectClassIdentifierOfC,
    EffectClassIdentifierOfH,
    EffectClassOf,
    InsClassOf,
    SigClassOf,
 )
import Control.Monad (forM)
import Data.Effect.Class.TH.Internal (
    EffectInfo,
    EffectOrder (FirstOrder, HigherOrder),
    IsDepParam,
    effName,
    effParamVars,
    effectType,
    isDepParam,
    tyVarKind,
    tyVarType,
 )
import Data.Foldable (fold)
import Language.Haskell.TH (
    Dec,
    Name,
    Q,
    TySynEqn (TySynEqn),
    TyVarBndr,
    Type,
    appT,
    conT,
    dataD,
    mkName,
    nameBase,
    promotedTupleT,
    tupleT,
    tySynInstD,
 )

generateEffectInfoTypeInstances :: EffectInfo -> Maybe (EffectOrder, Name) -> Q [Dec]
generateEffectInfoTypeInstances info mEffDataCls = do
    let eciName = eciNamer $ effName info
        indepParams = indepParamsIn $ effParamVars info
        depParams = depParamsIn $ effParamVars info
        eci = effectClassIdentifier eciName indepParams
        ec = effectType info
        depParamTuple = mkDepParamTuple depParams

    effInfoTyInsts <-
        sequence
            [ dataD (pure []) eciName indepParams Nothing [] []
            , tySynInstD $
                TySynEqn Nothing
                    <$> (conT ''EffectClassIdentifierOfC `appT` ec)
                    <*> eci
            , tySynInstD $
                TySynEqn Nothing
                    <$> (conT ''DepParams `appT` eci)
                    <*> tupleType (tyVarKind <$> depParams)
            , tySynInstD $
                TySynEqn Nothing
                    <$> (conT ''DepParamsOfC `appT` ec)
                    <*> depParamTuple
            , tySynInstD $
                TySynEqn Nothing
                    <$> (conT ''EffectClassOf `appT` eci `appT` depParamTuple)
                    <*> ec
            ]

    effDataInfoTyInsts <-
        forM mEffDataCls \(order, effDataClsName) -> do
            let dc =
                    foldl
                        appT
                        (conT effDataClsName)
                        (fmap tyVarType (fst <$> effParamVars info))

            sequence
                [ tySynInstD $
                    TySynEqn Nothing
                        <$> ( conT
                                ( case order of
                                    FirstOrder -> ''EffectClassIdentifierOf
                                    HigherOrder -> ''EffectClassIdentifierOfH
                                )
                                `appT` dc
                            )
                        <*> eci
                , tySynInstD $
                    TySynEqn Nothing
                        <$> ( conT
                                ( case order of
                                    FirstOrder -> ''DepParamsOf
                                    HigherOrder -> ''DepParamsOfH
                                )
                                `appT` dc
                            )
                        <*> depParamTuple
                , tySynInstD $
                    TySynEqn Nothing
                        <$> ( conT
                                ( case order of
                                    FirstOrder -> ''InsClassOf
                                    HigherOrder -> ''SigClassOf
                                )
                                `appT` eci
                                `appT` depParamTuple
                            )
                        <*> dc
                ]

    pure $ effInfoTyInsts <> fold effDataInfoTyInsts

eciNamer :: Name -> Name
eciNamer = mkName . ("I'" ++) . nameBase

tupleType :: [Q Type] -> Q Type
tupleType ts =
    foldl appT (tupleT $ length ts) ts

promotedTupleType :: [Q Type] -> Q Type
promotedTupleType ts =
    foldl appT (promotedTupleT $ length ts) ts

depParamsIn :: [(TyVarBndr (), IsDepParam)] -> [TyVarBndr ()]
depParamsIn = map fst . filter (isDepParam . snd)

indepParamsIn :: [(TyVarBndr (), IsDepParam)] -> [TyVarBndr ()]
indepParamsIn = map fst . filter (not . isDepParam . snd)

mkDepParamTuple :: [TyVarBndr ()] -> Q Type
mkDepParamTuple depParams = promotedTupleType (tyVarType <$> depParams)

effectClassIdentifier :: Name -> [TyVarBndr ()] -> Q Type
effectClassIdentifier eciName indepParams =
    foldl appT (conT eciName) (tyVarType <$> indepParams)
