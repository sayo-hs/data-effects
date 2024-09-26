{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Lens (Traversal', makeLenses, (%~), (.~), _head)
import Control.Monad (forM, forM_, replicateM, unless, when)
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
    addModFinalizer,
    nameBase,
    reify,
 )

import Control.Arrow ((>>>))
import Control.Effect (SendIns, SendSig, sendIns, sendSig)
import Control.Effect.Key (SendInsBy, SendSigBy, sendInsBy, sendSigBy)
import Control.Monad.Writer (WriterT, execWriterT, lift, tell)
import Data.Char (toLower)
import Data.Default (Default, def)
import Data.Effect (LiftIns (LiftIns))
import Data.Effect.Tag (Tag (Tag), TagH (TagH))
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.Either.Validation (Validation, eitherToValidation, validationToEither)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List.Extra (unsnoc)
import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
import Language.Haskell.TH (
    BangType,
    Body (NormalB),
    Clause (Clause),
    Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
    Dec (DataD, FunD, NewtypeD, PatSynD, PragmaD, TySynD),
    DocLoc (ArgDoc, DeclDoc),
    Exp (AppE, AppTypeE, ConE, SigE, VarE),
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
    Type (TupleT, WildCardT),
    getDoc,
    mkName,
    patSynSigD,
    pprint,
    putDoc,
    reportWarning,
 )
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Datatype.TyVarBndr (pattern BndrReq)

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

newtype MakeEffectConf = MakeEffectConf {unMakeEffectConf :: EffClsInfo -> Q EffectClassConf}

alterEffectClassConf :: (EffectClassConf -> EffectClassConf) -> MakeEffectConf -> MakeEffectConf
alterEffectClassConf f (MakeEffectConf conf) = MakeEffectConf (fmap f . conf)
{-# INLINE alterEffectClassConf #-}

alterEffectConf :: (EffectConf -> EffectConf) -> MakeEffectConf -> MakeEffectConf
alterEffectConf f = alterEffectClassConf \conf ->
    conf{_confByEffect = f . _confByEffect conf}

data EffectClassConf = EffectClassConf
    { _confByEffect :: Name -> EffectConf
    , _doesDeriveHFunctor :: Bool
    , _doesGenerateLiftInsTypeSynonym :: Bool
    , _doesGenerateLiftInsPatternSynonyms :: Bool
    }

data EffectConf = EffectConf
    { _normalSenderGenConf :: Maybe SenderFunctionConf
    , _taggedSenderGenConf :: Maybe SenderFunctionConf
    , _keyedSenderGenConf :: Maybe SenderFunctionConf
    , _warnFirstOrderInSigCls :: Bool
    }

data SenderFunctionConf = SenderFunctionConf
    { _senderFnName :: String
    , _doesGenerateSenderFnSignature :: Bool
    , _senderFnDoc :: Maybe String -> Q (Maybe String)
    , _senderFnArgDoc :: Int -> Maybe String -> Q (Maybe String)
    }

senderFnConfs :: Traversal' EffectConf SenderFunctionConf
senderFnConfs f EffectConf{..} = do
    normal <- traverse f _normalSenderGenConf
    tagged <- traverse f _taggedSenderGenConf
    keyed <- traverse f _keyedSenderGenConf
    pure
        EffectConf
            { _normalSenderGenConf = normal
            , _taggedSenderGenConf = tagged
            , _keyedSenderGenConf = keyed
            , _warnFirstOrderInSigCls
            }

makeLenses ''EffectClassConf
makeLenses ''EffectConf
makeLenses ''SenderFunctionConf

deriveHFunctor :: MakeEffectConf -> MakeEffectConf
deriveHFunctor = alterEffectClassConf $ doesDeriveHFunctor .~ True
{-# INLINE deriveHFunctor #-}

noDeriveHFunctor :: MakeEffectConf -> MakeEffectConf
noDeriveHFunctor = alterEffectClassConf $ doesDeriveHFunctor .~ False
{-# INLINE noDeriveHFunctor #-}

generateLiftInsTypeSynonym :: MakeEffectConf -> MakeEffectConf
generateLiftInsTypeSynonym = alterEffectClassConf $ doesGenerateLiftInsTypeSynonym .~ True
{-# INLINE generateLiftInsTypeSynonym #-}

noGenerateLiftInsTypeSynonym :: MakeEffectConf -> MakeEffectConf
noGenerateLiftInsTypeSynonym = alterEffectClassConf $ doesGenerateLiftInsTypeSynonym .~ False
{-# INLINE noGenerateLiftInsTypeSynonym #-}

generateLiftInsPatternSynonyms :: MakeEffectConf -> MakeEffectConf
generateLiftInsPatternSynonyms = alterEffectClassConf $ doesGenerateLiftInsPatternSynonyms .~ True
{-# INLINE generateLiftInsPatternSynonyms #-}

noGenerateLiftInsPatternSynonyms :: MakeEffectConf -> MakeEffectConf
noGenerateLiftInsPatternSynonyms =
    alterEffectClassConf $ doesGenerateLiftInsPatternSynonyms .~ False
{-# INLINE noGenerateLiftInsPatternSynonyms #-}

noGenerateNormalSenderFunction :: MakeEffectConf -> MakeEffectConf
noGenerateNormalSenderFunction = alterEffectConf $ normalSenderGenConf .~ Nothing
{-# INLINE noGenerateNormalSenderFunction #-}

noGenerateTaggedSenderFunction :: MakeEffectConf -> MakeEffectConf
noGenerateTaggedSenderFunction = alterEffectConf $ taggedSenderGenConf .~ Nothing
{-# INLINE noGenerateTaggedSenderFunction #-}

noGenerateKeyedSenderFunction :: MakeEffectConf -> MakeEffectConf
noGenerateKeyedSenderFunction = alterEffectConf $ keyedSenderGenConf .~ Nothing
{-# INLINE noGenerateKeyedSenderFunction #-}

suppressFirstOrderInSignatureClassWarning :: MakeEffectConf -> MakeEffectConf
suppressFirstOrderInSignatureClassWarning = alterEffectConf $ warnFirstOrderInSigCls .~ False
{-# INLINE suppressFirstOrderInSignatureClassWarning #-}

noGenerateSenderFunctionSignature :: MakeEffectConf -> MakeEffectConf
noGenerateSenderFunctionSignature =
    alterEffectConf $ senderFnConfs %~ doesGenerateSenderFnSignature .~ False
{-# INLINE noGenerateSenderFunctionSignature #-}

instance Default MakeEffectConf where
    def = MakeEffectConf $ const $ pure def
    {-# INLINE def #-}

instance Default EffectClassConf where
    def =
        EffectClassConf
            { _confByEffect = \effConName ->
                let normalSenderFnConf =
                        SenderFunctionConf
                            { _senderFnName =
                                let effConName' = nameBase effConName
                                 in if head effConName' == ':'
                                        then tail effConName'
                                        else effConName' & _head %~ toLower
                            , _doesGenerateSenderFnSignature = True
                            , _senderFnDoc = pure
                            , _senderFnArgDoc = const pure
                            }
                 in EffectConf
                        { _normalSenderGenConf = Just normalSenderFnConf
                        , _taggedSenderGenConf =
                            Just $ normalSenderFnConf & senderFnName %~ (++ "'")
                        , _keyedSenderGenConf =
                            Just $ normalSenderFnConf & senderFnName %~ (++ "''")
                        , _warnFirstOrderInSigCls = True
                        }
            , _doesDeriveHFunctor = True
            , _doesGenerateLiftInsTypeSynonym = True
            , _doesGenerateLiftInsPatternSynonyms = True
            }

genSenders :: EffectClassConf -> EffClsInfo -> Q [Dec]
genSenders EffectClassConf{..} ec@EffClsInfo{..} = do
    let order = orderOf ec

    execWriterT $ forM ecEffs \con@EffConInfo{..} -> do
        let EffectConf{..} = _confByEffect effName

        forM_ _normalSenderGenConf \conf -> genNormalSender order conf con
        forM_ _taggedSenderGenConf \conf -> genTaggedSender order conf con
        forM_ _keyedSenderGenConf \conf -> genKeyedSender order conf con

        -- Check for First Order in Signature Class warning
        when (_warnFirstOrderInSigCls && order == HigherOrder) do
            let isHigherOrderEffect = any (tyVarName (fromJust effCarrier) `occurs`) effParamTypes

            unless isHigherOrderEffect do
                lift $
                    reportWarning $
                        "The first-order effect ‘"
                            <> nameBase effName
                            <> "’ has been found within the signature class data type ‘"
                            <> nameBase ecName
                            <> "’.\nConsider separating the first-order effect into an instruction class data type."

genNormalSender
    :: EffectOrder
    -> SenderFunctionConf
    -> EffConInfo
    -> WriterT [Dec] Q ()
genNormalSender order = genSender order send sendCxt id
  where
    (send, sendCxt) = case order of
        FirstOrder ->
            ( (VarE 'sendIns `AppE`)
            , \effDataType carrier -> ConT ''SendIns `AppT` effDataType `AppT` carrier
            )
        HigherOrder ->
            ( (VarE 'sendSig `AppE`)
            , \effDataType carrier -> ConT ''SendSig `AppT` effDataType `AppT` carrier
            )

genTaggedSender
    :: EffectOrder
    -> SenderFunctionConf
    -> EffConInfo
    -> WriterT [Dec] Q ()
genTaggedSender order conf eff = do
    nTag <- newName "tag" & lift
    let tag = VarT nTag

        (send, sendCxt) = case order of
            FirstOrder ->
                ( (VarE 'sendIns `AppE`) . (ConE 'Tag `AppTypeE` WildCardT `AppTypeE` tag `AppE`)
                , \effDataType carrier ->
                    ConT ''SendIns `AppT` (ConT ''Tag `AppT` effDataType `AppT` tag) `AppT` carrier
                )
            HigherOrder ->
                ( (VarE 'sendSig `AppE`) . (ConE 'TagH `AppTypeE` WildCardT `AppTypeE` tag `AppE`)
                , \effDataType carrier ->
                    ConT ''SendSig `AppT` (ConT ''TagH `AppT` effDataType `AppT` tag) `AppT` carrier
                )

    genSender order send sendCxt (PlainTV nTag SpecifiedSpec :) conf eff

genKeyedSender
    :: EffectOrder
    -> SenderFunctionConf
    -> EffConInfo
    -> WriterT [Dec] Q ()
genKeyedSender order conf eff = do
    nKey <- newName "key" & lift
    let key = VarT nKey

        (send, sendCxt) = case order of
            FirstOrder ->
                ( (VarE 'sendInsBy `AppTypeE` key `AppE`)
                , \effDataType carrier ->
                    ConT ''SendInsBy `AppT` key `AppT` effDataType `AppT` carrier
                )
            HigherOrder ->
                ( (VarE 'sendSigBy `AppTypeE` key `AppE`)
                , \effDataType carrier ->
                    ConT ''SendSigBy `AppT` key `AppT` effDataType `AppT` carrier
                )

    genSender order send sendCxt (PlainTV nKey SpecifiedSpec :) conf eff

genSender
    :: EffectOrder
    -> (Exp -> Exp)
    -> (TH.Type -> TH.Type -> TH.Type)
    -> ([TyVarBndrSpec] -> [TyVarBndrSpec])
    -> SenderFunctionConf
    -> EffConInfo
    -> WriterT [Dec] Q ()
genSender order send sendCxt alterFnSigTVs conf@SenderFunctionConf{..} con@EffConInfo{..} = do
    genSenderArmor sendCxt alterFnSigTVs conf con \f -> do
        args <- replicateM (length effParamTypes) (newName "x")

        let body =
                send
                    ( foldl' AppE (ConE effName) (map VarE args)
                        & if _doesGenerateSenderFnSignature
                            then (`SigE` ((effDataType & appCarrier) `AppT` effResultType))
                            else id
                    )

            appCarrier = case order of
                FirstOrder -> id
                HigherOrder -> (`AppT` f)

        pure $ Clause (map VarP args) (NormalB body) []

genSenderArmor
    :: (TH.Type -> TH.Type -> TH.Type)
    -> ([TyVarBndrSpec] -> [TyVarBndrSpec])
    -> SenderFunctionConf
    -> EffConInfo
    -> (Type -> Q Clause)
    -> WriterT [Dec] Q ()
genSenderArmor sendCxt alterFnSigTVs SenderFunctionConf{..} EffConInfo{..} clause = do
    carrier <- maybe ((`PlainTV` ()) <$> newName "f") pure effCarrier & lift

    let f = tyVarType carrier

        fnName = mkName _senderFnName

        funSig =
            SigD
                fnName
                ( ForallT
                    (effTyVars ++ [carrier $> SpecifiedSpec] & alterFnSigTVs)
                    (sendCxt effDataType f : effCxt)
                    (arrowChain effParamTypes (f `AppT` effResultType))
                )

        funInline = PragmaD (InlineP fnName Inline FunLike AllPhases)

    funDef <- FunD fnName <$> sequence [clause f & lift]

    -- Put documents
    lift do
        effDoc <- getDoc $ DeclDoc effName
        _senderFnDoc effDoc >>= mapM_ \doc -> do
            addModFinalizer $ putDoc (DeclDoc fnName) doc

        forM [0 .. length effParamTypes - 1] \i -> do
            argDoc <- getDoc $ ArgDoc effName i
            _senderFnArgDoc i argDoc >>= mapM_ \doc -> do
                addModFinalizer $ putDoc (ArgDoc fnName i) doc

    -- Append declerations
    when _doesGenerateSenderFnSignature $ tell [funSig]
    tell [funDef, funInline]

arrowChain :: (Foldable t) => t TH.Type -> TH.Type -> TH.Type
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
            decomposeGadtReturnType
                :: TH.Type -> Either [T.Text] (TH.Type, Maybe (TyVarBndr ()), TH.Type)
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
                        sig `AppT` SigT (VarT f) kf `AppT` x ->
                            Right (sig, Just (KindedTV f () kf), x)
                        sig `AppT` VarT f `AppT` x ->
                            Right (sig, Just (PlainTV f ()), x)
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
                            ()
                            => ( $(pure a) ~ $(pure effResultType)
                               , $(pure $ foldl AppT (TupleT (length effCxt)) effCxt)
                               )
                            => $( pure $
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
        (pvs <&> (`PlainTV` BndrReq))
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
    TyConI (NewtypeD cxt name args _ constr _) ->
        Just $ DataInfo cxt name (map ($> ()) args) (normalizeCon constr)
    TyConI (DataD cxt name args _ constrs _) ->
        Just $ DataInfo cxt name (map ($> ()) args) (concatMap normalizeCon constrs)
    _ -> Nothing

normalizeCon :: Con -> [ConInfo]
normalizeCon = \case
    ForallC vars cxt constr ->
        [con{conTyVars = vars, conCxt = cxt} | con <- normalizeNonForallCon constr]
    con -> normalizeNonForallCon con

normalizeNonForallCon :: Con -> [ConInfo]
normalizeNonForallCon = \case
    NormalC constr args -> [ConInfo constr args Nothing [] []]
    RecC constr args -> [ConInfo constr (args <&> \(_, s, t) -> (s, t)) Nothing [] []]
    InfixC a constr b -> [ConInfo constr [a, b] Nothing [] []]
    GadtC cons args typ -> [ConInfo con args (Just typ) [] [] | con <- cons]
    RecGadtC cons args typ ->
        [ConInfo con (args <&> \(_, s, t) -> (s, t)) (Just typ) [] [] | con <- cons]
    ForallC{} -> fail "Unexpected nested forall."
