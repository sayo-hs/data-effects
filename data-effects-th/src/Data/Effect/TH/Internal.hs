{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
               (c) 2010-2011 Patrick Bahr, Tom Hvitved
               (c) 2020 Michael Szvetits
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.TH.Internal where

import Control.Lens ((%~), _head)
import Control.Monad (forM, replicateM, unless, when)
import Data.List (foldl')
import Language.Haskell.TH.Syntax (
    Con,
    Cxt,
    Dec (SigD),
    Info,
    Name,
    Q,
    Quote (newName),
    TyVarBndr,
    Type (
        AppKindT,
        AppT,
        ArrowT,
        ConT,
        ForallT,
        ImplicitParamT,
        InfixT,
        ParensT,
        PromotedT,
        SigT,
        UInfixT,
        VarT
    ),
    nameBase,
    reify,
 )

import Control.Arrow ((>>>))
import Control.Effect (SendIns, SendSig, sendIns, sendSig)
import Data.Char (toLower)
import Data.Default (Default, def)
import Data.Effect (LiftIns (LiftIns))
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.Either.Validation (Validation, eitherToValidation, validationToEither)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List.Extra (unsnoc)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
import Language.Haskell.TH (
    BangType,
    Body (NormalB),
    Clause (Clause),
    Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
    Dec (DataD, FunD, NewtypeD, PatSynD, PragmaD, TySynD),
    Exp (AppE, ConE, SigE, VarE),
    Info (TyConI),
    Inline (Inline),
    Pat (ConP, VarP),
    PatSynArgs (PrefixPatSyn),
    PatSynDir (ImplBidir),
    Phases (AllPhases),
    Pragma (CompleteP, InlineP),
    RuleMatch (FunLike),
    Specificity (SpecifiedSpec),
    TyVarBndr (..),
    TyVarBndrSpec,
    mkName,
    patSynSigD,
    pprint,
    reportWarning,
 )
import Language.Haskell.TH qualified as TH

data EffClsInfo = EffClsInfo
    { ecName :: Name
    , ecParamVars :: [TyVarBndr ()]
    , ecCarrier :: Maybe (TyVarBndr ())
    , ecEffs :: [EffConInfo]
    }

data EffConInfo = EffConInfo
    { effName :: Name
    , effParamTypes :: [TH.Type]
    , effDataType :: TH.Type
    , effResultType :: TH.Type
    , effTyVars :: [TyVarBndrSpec]
    , effCarrier :: Maybe (TyVarBndr ())
    , effCxt :: Cxt
    }

-- | An order of effect.
data EffectOrder = FirstOrder | HigherOrder
    deriving (Show, Eq, Ord)

orderOf :: EffClsInfo -> EffectOrder
orderOf =
    ecCarrier >>> \case
        Just _ -> HigherOrder
        Nothing -> FirstOrder

makeSenderAll :: MakeEffectConf -> EffClsInfo -> Q [Dec]
makeSenderAll conf =
    makeSender conf
        >>> fmap
            ( concatMap \(fnSig, fn) ->
                ( if genSenderFnSignature conf
                    then (fnSig :)
                    else id
                )
                    fn
            )

data MakeEffectConf = MakeEffectConf
    { warnFirstOrderInSigCls :: Bool
    , genSenderFnSignature :: Bool
    }

instance Default MakeEffectConf where
    def =
        MakeEffectConf
            { warnFirstOrderInSigCls = True
            , genSenderFnSignature = True
            }

makeSender :: MakeEffectConf -> EffClsInfo -> Q (Map Name (Dec, [Dec]))
makeSender MakeEffectConf{..} ec@EffClsInfo{..} = do
    Map.fromList <$> forM ecEffs \EffConInfo{..} -> do
        args <- replicateM (length effParamTypes) (newName "x")

        carrier <- maybe ((`PlainTV` ()) <$> newName "f") pure effCarrier
        let f = tyVarType carrier

            appCarrier = case order of
                FirstOrder -> id
                HigherOrder -> (`AppT` f)

            funName = renameConToFun effName
            body =
                VarE sendMethod
                    `AppE` ( foldl' AppE con (map VarE args)
                                & if genSenderFnSignature
                                    then (`SigE` (appCarrier effDataType `AppT` effResultType))
                                    else id
                           )
            con = ConE effName

            funSig =
                SigD
                    funName
                    ( ForallT
                        (effTyVars ++ [carrier $> SpecifiedSpec])
                        (ConT sendCxt `AppT` effDataType `AppT` f : effCxt)
                        (arrowChain effParamTypes (f `AppT` effResultType))
                    )

            funDef = FunD (renameConToFun effName) [Clause (map VarP args) (NormalB body) []]
            funInline = PragmaD (InlineP funName Inline FunLike AllPhases)

        when (warnFirstOrderInSigCls && order == HigherOrder) do
            let isHigherOrderEffect = any (tyVarName (fromJust effCarrier) `occurs`) effParamTypes
            unless isHigherOrderEffect do
                reportWarning $
                    "The first-order effect ‘"
                        <> nameBase effName
                        <> "’ has been found within the signature class data type ‘"
                        <> nameBase ecName
                        <> "’.\nConsider separating the first-order effect into an instruction class data type."

        pure (effName, (funSig, [funDef, funInline]))
  where
    (sendMethod, sendCxt) = case order of
        FirstOrder -> ('sendIns, ''SendIns)
        HigherOrder -> ('sendSig, ''SendSig)

    order = orderOf ec

renameConToFun :: Name -> Name
renameConToFun = mkName . (_head %~ toLower) . nameBase

arrowChain :: Foldable t => t TH.Type -> TH.Type -> TH.Type
arrowChain = flip $ foldr \l r -> ArrowT `AppT` l `AppT` r

-- | A reified information of a datatype.
data DataInfo = DataInfo
    { dataCxt :: Cxt
    , dataName :: Name
    , dataTyVars :: [TyVarBndr ()]
    , dataCons :: [ConInfo]
    }

data ConInfo = ConInfo
    { conName :: Name
    , conArgs :: [BangType]
    , conGadtReturnType :: Maybe TH.Type
    , conTyVars :: [TyVarBndrSpec]
    , conCxt :: Cxt
    }

reifyEffCls :: EffectOrder -> Name -> Q (Info, DataInfo, EffClsInfo)
reifyEffCls order name = do
    info <- reify name

    dataInfo <-
        analyzeData info
            & maybe (fail $ "Not datatype: ‘" <> pprint name <> "’") pure

    effClsInfo <-
        analyzeEffCls order dataInfo
            & either (fail . T.unpack) pure

    pure (info, dataInfo, effClsInfo)

analyzeEffCls :: EffectOrder -> DataInfo -> Either T.Text EffClsInfo
analyzeEffCls order DataInfo{..} = do
    (initTyVars, resultType) <- unsnoc dataTyVars & maybeToEither "No result type variable."

    (paramVars, mCarrier) <-
        case order of
            FirstOrder -> pure (initTyVars, Nothing)
            HigherOrder -> do
                (pvs, carrier) <- unsnoc initTyVars & maybeToEither "No carrier type variable."
                pure (pvs, Just carrier)

    let analyzeEffCon :: ConInfo -> Validation [T.Text] EffConInfo
        analyzeEffCon ConInfo{..} = eitherToValidation do
            (effDataType, effCarrier, effResultType) <-
                maybe
                    ( pure
                        ( foldl' AppT (VarT dataName) (map tyVarType paramVars)
                        , mCarrier
                        , tyVarType resultType
                        )
                    )
                    decomposeGadtReturnType
                    conGadtReturnType

            let removeCarrierTV :: [TyVarBndr a] -> [TyVarBndr a]
                removeCarrierTV = case order of
                    FirstOrder -> id
                    HigherOrder -> filter ((tyVarName <$> effCarrier /=) . Just . tyVarName)

                effTyVars =
                    if isJust conGadtReturnType
                        then removeCarrierTV conTyVars
                        else map (SpecifiedSpec <$) (removeCarrierTV paramVars) ++ conTyVars

            Right
                EffConInfo
                    { effName = conName
                    , effParamTypes = map snd conArgs
                    , effDataType = effDataType
                    , effResultType = effResultType
                    , effTyVars = effTyVars
                    , effCarrier = effCarrier
                    , effCxt = conCxt
                    }
          where
            decomposeGadtReturnType :: TH.Type -> Either [T.Text] (TH.Type, Maybe (TyVarBndr ()), TH.Type)
            decomposeGadtReturnType =
                unkindType >>> case order of
                    FirstOrder ->
                        \case
                            ins `AppT` x -> Right (ins, Nothing, x)
                            t ->
                                Left
                                    [ "Unexpected form of GADT return type for the instruction ‘"
                                        <> T.pack (nameBase conName)
                                        <> "’: "
                                        <> T.pack (pprint t)
                                    ]
                    HigherOrder -> \case
                        sig `AppT` SigT (VarT f) kf `AppT` x -> Right (sig, Just (KindedTV f () kf), x)
                        sig `AppT` VarT f `AppT` x -> Right (sig, Just (PlainTV f ()), x)
                        t ->
                            Left
                                [ "Unexpected form of GADT return type for the signature ‘"
                                    <> T.pack (nameBase conName)
                                    <> "’: "
                                    <> T.pack (pprint t)
                                ]

    effCons <-
        traverse analyzeEffCon dataCons
            & validationToEither
            & mapLeft T.unlines

    pure
        EffClsInfo
            { ecName = dataName
            , ecParamVars = paramVars
            , ecCarrier = mCarrier
            , ecEffs = effCons
            }

-- ** Generating Synonyms about LiftIns

{- |
Generate the pattern synonyms for instruction constructors:

    @pattern LBaz ... = LiftIns (Baz ...)@
-}
genLiftInsPatternSynonyms :: EffClsInfo -> Q [Dec]
genLiftInsPatternSynonyms EffClsInfo{..} = do
    patSyns <-
        forM ecEffs \EffConInfo{..} -> do
            let newConName = mkName $ 'L' : nameBase effName
            args <- replicateM (length effParamTypes) (newName "x")

            f <- VarT <$> newName "f"
            a <- VarT <$> newName "a"

            (newConName,)
                <$> sequence
                    [ patSynSigD
                        newConName
                        -- For some reason, if I don't write constraints in this form, the type is
                        -- not inferred properly (why?).
                        [t|
                            () =>
                            ($(pure a) ~ $(pure effResultType)) =>
                            $( pure $
                                arrowChain
                                    effParamTypes
                                    ((ConT ''LiftIns `AppT` effDataType) `AppT` f `AppT` a)
                             )
                            |]
                    , pure $
                        PatSynD
                            newConName
                            (PrefixPatSyn args)
                            ImplBidir
                            (ConP 'LiftIns [] [ConP effName [] (VarP <$> args)])
                    ]

    pure $ concatMap snd patSyns ++ [PragmaD $ CompleteP (fst <$> patSyns) Nothing]

{- |
Generate the type synonym for an instruction class datatype:

    @type (LFoobar ...) = LiftIns (Foobar ...)@
-}
genLiftInsTypeSynonym :: EffClsInfo -> Dec
genLiftInsTypeSynonym EffClsInfo{..} = do
    TySynD
        (mkName $ 'L' : nameBase ecName)
        (pvs <&> (`PlainTV` ()))
        (ConT ''LiftIns `AppT` foldl AppT (ConT ecName) (map VarT pvs))
  where
    pvs = tyVarName <$> ecParamVars

-- * Utility functions

{-  The code before modification is licensed under the BSD3 License as
    shown in [1]. The modified code, in its entirety, is licensed under
    MPL 2.0. When redistributing, please ensure that you do not remove
    the BSD3 License text as indicated in [1].
    <https://hackage.haskell.org/package/effet-0.4.0.0/docs/src/Control.Effect.Machinery.TH.html>

    [1] Copyright Michael Szvetits (c) 2020

        All rights reserved.

        Redistribution and use in source and binary forms, with or without
        modification, are permitted provided that the following conditions are met:

            * Redistributions of source code must retain the above copyright
            notice, this list of conditions and the following disclaimer.

            * Redistributions in binary form must reproduce the above
            copyright notice, this list of conditions and the following
            disclaimer in the documentation and/or other materials provided
            with the distribution.

            * Neither the name of Michael Szvetits nor the names of other
            contributors may be used to endorse or promote products derived
            from this software without specific prior written permission.

        THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
        "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
        LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
        A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
        OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
        SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
        LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
        DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
        THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
        (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
        OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

-- | Pures the name of a type variable.
tyVarName :: TyVarBndr a -> Name
tyVarName (PlainTV n _) = n
tyVarName (KindedTV n _ _) = n

-- | Converts a type variable to a type.
tyVarType :: TyVarBndr a -> TH.Type
tyVarType (PlainTV n _) = VarT n
tyVarType (KindedTV n _ k) = SigT (VarT n) k

-- | Throws away all kind information from a type.
unkindTypeRec :: TH.Type -> TH.Type
unkindTypeRec = \case
    ForallT vs ps t -> ForallT (fmap unkindTyVar vs) (fmap unkindTypeRec ps) (unkindTypeRec t)
    AppT l r -> AppT (unkindTypeRec l) (unkindTypeRec r)
    SigT t _ -> t
    InfixT l n r -> InfixT (unkindTypeRec l) n (unkindTypeRec r)
    UInfixT l n r -> UInfixT (unkindTypeRec l) n (unkindTypeRec r)
    ParensT t -> ParensT (unkindTypeRec t)
    AppKindT t _ -> unkindTypeRec t
    ImplicitParamT s t -> ImplicitParamT s (unkindTypeRec t)
    other -> other

unkindType :: TH.Type -> TH.Type
unkindType = \case
    SigT t _ -> t
    other -> other

-- | Throws away the kind information of a type variable.
unkindTyVar :: TyVarBndr a -> TyVarBndr a
unkindTyVar (KindedTV n s _) = PlainTV n s
unkindTyVar unkinded = unkinded

-- | Checks if a name m appears somewhere in a type.
occurs :: Name -> TH.Type -> Bool
occurs m = \case
    ForallT _ cxt t -> m `occurs` t || any (m `occurs`) cxt
    AppT l r -> m `occurs` l || m `occurs` r
    SigT t _ -> m `occurs` t
    VarT n -> n == m
    ConT n -> n == m
    PromotedT n -> n == m
    InfixT l n r -> n == m || m `occurs` l || m `occurs` r
    UInfixT l n r -> n == m || m `occurs` l || m `occurs` r
    ParensT t -> m `occurs` t
    AppKindT t _ -> m `occurs` t
    ImplicitParamT _ t -> m `occurs` t
    _ -> False

{-  The code before modification is licensed under the BSD3 License as
    shown in [1].  The modified code, in its entirety, is licensed under
    MPL 2.0. When redistributing, please ensure that you do not remove
    the BSD3 License text as indicated in [2].
    <https://github.com/pa-ba/compdata/blob/master/src/Data/Comp/Derive/Utils.hs>

    [2] Copyright (c) 2010--2011 Patrick Bahr, Tom Hvitved

        All rights reserved.

        Redistribution and use in source and binary forms, with or without
        modification, are permitted provided that the following conditions
        are met:

        1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

        2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

        3. Neither the name of the author nor the names of his contributors
        may be used to endorse or promote products derived from this software
        without specific prior written permission.

        THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
        IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
        WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
        DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
        ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
        DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
        OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
        HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
        STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
        ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
        POSSIBILITY OF SUCH DAMAGE.
-}

{- |
This function abstracts away @newtype@ declaration, it turns them into
@data@ declarations.
-}
analyzeData :: Info -> Maybe DataInfo
analyzeData = \case
    TyConI (NewtypeD cxt name args _ constr _) -> Just $ DataInfo cxt name args (normalizeCon constr)
    TyConI (DataD cxt name args _ constrs _) -> Just $ DataInfo cxt name args (concatMap normalizeCon constrs)
    _ -> Nothing

normalizeCon :: Con -> [ConInfo]
normalizeCon = \case
    ForallC vars cxt constr -> [con{conTyVars = vars, conCxt = cxt} | con <- normalizeNonForallCon constr]
    con -> normalizeNonForallCon con

normalizeNonForallCon :: Con -> [ConInfo]
normalizeNonForallCon = \case
    NormalC constr args -> [ConInfo constr args Nothing [] []]
    RecC constr args -> [ConInfo constr (args <&> \(_, s, t) -> (s, t)) Nothing [] []]
    InfixC a constr b -> [ConInfo constr [a, b] Nothing [] []]
    GadtC cons args typ -> [ConInfo con args (Just typ) [] [] | con <- cons]
    RecGadtC cons args typ -> [ConInfo con (args <&> \(_, s, t) -> (s, t)) (Just typ) [] [] | con <- cons]
    ForallC{} -> fail "Unexpected nested forall."
