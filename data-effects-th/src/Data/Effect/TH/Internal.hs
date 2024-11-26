{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
               (c) 2010-2011 Patrick Bahr, Tom Hvitved
               (c) 2020 Michael Szvetits
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.TH.Internal where

import Control.Arrow ((>>>))
import Control.Effect (Perform, perform)
import Control.Effect.Key (PerformBy, performBy)
import Control.Lens (Traversal', makeLenses, (%~), (.~), _head)
import Control.Monad (forM, forM_, replicateM, when)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Writer.CPS (WriterT, execWriterT, lift, tell)
import Data.Char (toLower)
import Data.Default (Default, def)
import Data.Effect (EffectOrder (FirstOrder, HigherOrder), FirstOrder, OrderOf)
import Data.Effect.Tag (Tagged (Tagged))
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.Either.Validation (Validation, eitherToValidation, validationToEither)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List (foldl', uncons)
import Data.List.Extra (unsnoc)
import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
import Language.Haskell.TH (
    BangType,
    Body (NormalB),
    Clause (Clause),
    Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
    Dec (DataD, FunD, NewtypeD, PragmaD),
    DocLoc (ArgDoc, DeclDoc),
    Exp (AppE, AppTypeE, ConE, SigE, VarE),
    Info (TyConI),
    Inline (Inline),
    Pat (VarP),
    Phases (AllPhases),
    Pragma (InlineP),
    RuleMatch (FunLike),
    Specificity (SpecifiedSpec),
    TyVarBndr (..),
    TyVarBndrSpec,
    Type (WildCardT),
    getDoc,
    mkName,
    pprint,
    putDoc,
 )
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (
    Cxt,
    Dec (SigD),
    Name,
    Q,
    Quote (newName),
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

data EffectInfo = EffectInfo
    { eName :: Name
    , eParamVars :: [TyVarBndr ()]
    , eCarrier :: TyVarBndr ()
    , eOps :: [OpInfo]
    }

data OpInfo = OpInfo
    { opName :: Name
    , opParamTypes :: [TH.Type]
    , opDataType :: TH.Type
    , opResultType :: TH.Type
    , opTyVars :: [TyVarBndrSpec]
    , opCarrier :: TyVarBndr ()
    , opCxt :: Cxt
    , opOrder :: EffectOrder
    }

newtype EffectConf = EffectConf {opConf :: Name -> OpConf}

alterOpConf :: (OpConf -> OpConf) -> EffectConf -> EffectConf
alterOpConf f conf = conf{opConf = f . opConf conf}

data OpConf = OpConf
    { _normalSenderGenConf :: Maybe SenderFunctionConf
    , _keyedSenderGenConf :: Maybe SenderFunctionConf
    , _taggedSenderGenConf :: Maybe SenderFunctionConf
    }

data SenderFunctionConf = SenderFunctionConf
    { _senderFnName :: String
    , _doesGenerateSenderFnSignature :: Bool
    , _senderFnDoc :: Maybe String -> Q (Maybe String)
    , _senderFnArgDoc :: Int -> Maybe String -> Q (Maybe String)
    }

senderFnConfs :: Traversal' OpConf SenderFunctionConf
senderFnConfs f OpConf{..} = do
    normal <- traverse f _normalSenderGenConf
    keyed <- traverse f _keyedSenderGenConf
    tagged <- traverse f _taggedSenderGenConf
    pure
        OpConf
            { _normalSenderGenConf = normal
            , _keyedSenderGenConf = keyed
            , _taggedSenderGenConf = tagged
            }

makeLenses ''OpConf
makeLenses ''SenderFunctionConf

noGenerateNormalSenderFunction :: EffectConf -> EffectConf
noGenerateNormalSenderFunction = alterOpConf $ normalSenderGenConf .~ Nothing
{-# INLINE noGenerateNormalSenderFunction #-}

noGenerateKeyedSenderFunction :: EffectConf -> EffectConf
noGenerateKeyedSenderFunction = alterOpConf $ keyedSenderGenConf .~ Nothing
{-# INLINE noGenerateKeyedSenderFunction #-}

noGenerateTaggedSenderFunction :: EffectConf -> EffectConf
noGenerateTaggedSenderFunction = alterOpConf $ taggedSenderGenConf .~ Nothing
{-# INLINE noGenerateTaggedSenderFunction #-}

noGenerateSenderFunctionSignature :: EffectConf -> EffectConf
noGenerateSenderFunctionSignature =
    alterOpConf $ senderFnConfs %~ doesGenerateSenderFnSignature .~ False
{-# INLINE noGenerateSenderFunctionSignature #-}

instance Default EffectConf where
    def =
        EffectConf
            { opConf = \effConName ->
                let normalSenderFnConf =
                        SenderFunctionConf
                            { _senderFnName =
                                let effConName' = nameBase effConName
                                    (opNameInitial, opNameTail) = fromJust $ uncons effConName'
                                 in if opNameInitial == ':'
                                        then opNameTail
                                        else effConName' & _head %~ toLower
                            , _doesGenerateSenderFnSignature = True
                            , _senderFnDoc = pure
                            , _senderFnArgDoc = const pure
                            }
                 in OpConf
                        { _normalSenderGenConf = Just normalSenderFnConf
                        , _keyedSenderGenConf =
                            Just $ normalSenderFnConf & senderFnName %~ (++ "'")
                        , _taggedSenderGenConf =
                            Just $ normalSenderFnConf & senderFnName %~ (++ "''")
                        }
            }

type EffectGenerator =
    ReaderT (EffectConf, Name, Info, DataInfo, EffectInfo) (WriterT [Dec] Q) ()

genEffect, genFOE, genHOE :: EffectGenerator
genEffect = do
    (conf, _, _, _, eInfo) <- ask
    genSenders conf eInfo & lift & lift >>= tell
genFOE = do
    genEffect
    (_, _, _, _, EffectInfo{..}) <- ask
    let eData = foldl AppT (ConT eName) (map (VarT . tyVarName) eParamVars)
    [d|type instance OrderOf $(pure eData) = 'FirstOrder|] & lift & lift >>= tell
    [d|instance FirstOrder $(pure eData)|] & lift & lift >>= tell
genHOE = do
    genEffect
    (_, _, _, _, EffectInfo{..}) <- ask
    let eData = foldl AppT (ConT eName) (map (VarT . tyVarName) eParamVars)
    [d|type instance OrderOf $(pure eData) = HigherOrder|] & lift & lift >>= tell

genSenders :: EffectConf -> EffectInfo -> Q [Dec]
genSenders EffectConf{..} EffectInfo{..} = do
    execWriterT $ forM eOps \con@OpInfo{..} -> do
        let OpConf{..} = opConf opName

        forM_ _normalSenderGenConf (genNormalSender con)
        forM_ _keyedSenderGenConf (genKeyedSender con)
        forM_ _taggedSenderGenConf (genTaggedSender con)

genNormalSender
    :: OpInfo
    -> SenderFunctionConf
    -> WriterT [Dec] Q ()
genNormalSender =
    genSender
        (VarE 'perform `AppE`)
        (\opDataType carrier -> ConT ''Perform `AppT` opDataType `AppT` carrier)
        id

genKeyedSender
    :: OpInfo
    -> SenderFunctionConf
    -> WriterT [Dec] Q ()
genKeyedSender eff conf = do
    nKey <- newName "key" & lift
    let key = VarT nKey

    genSender
        (VarE 'performBy `AppTypeE` key `AppE`)
        ( \opDataType carrier ->
            ConT ''PerformBy `AppT` key `AppT` opDataType `AppT` carrier
        )
        (PlainTV nKey SpecifiedSpec :)
        eff
        conf

genTaggedSender
    :: OpInfo
    -> SenderFunctionConf
    -> WriterT [Dec] Q ()
genTaggedSender conf eff = do
    nTag <- newName "tag" & lift
    let tag = VarT nTag

    genSender
        ((VarE 'perform `AppE`) . (ConE 'Tagged `AppTypeE` WildCardT `AppTypeE` tag `AppE`))
        ( \opDataType carrier ->
            ConT ''Perform `AppT` (ConT ''Tagged `AppT` opDataType `AppT` tag) `AppT` carrier
        )
        (PlainTV nTag SpecifiedSpec :)
        conf
        eff

genSender
    :: (Exp -> Exp)
    -> (TH.Type -> TH.Type -> TH.Type)
    -> ([TyVarBndrSpec] -> [TyVarBndrSpec])
    -> OpInfo
    -> SenderFunctionConf
    -> WriterT [Dec] Q ()
genSender send sendCxt alterFnSigTVs con@OpInfo{..} conf@SenderFunctionConf{..} = do
    genSenderArmor sendCxt alterFnSigTVs con conf \f -> do
        args <- replicateM (length opParamTypes) (newName "x")

        let body =
                send
                    ( foldl' AppE (ConE opName) (map VarE args)
                        & if _doesGenerateSenderFnSignature
                            then (`SigE` ((opDataType `AppT` f) `AppT` opResultType))
                            else id
                    )

        pure $ Clause (map VarP args) (NormalB body) []

genSenderArmor
    :: (TH.Type -> TH.Type -> TH.Type)
    -> ([TyVarBndrSpec] -> [TyVarBndrSpec])
    -> OpInfo
    -> SenderFunctionConf
    -> (Type -> Q Clause)
    -> WriterT [Dec] Q ()
genSenderArmor sendCxt alterFnSigTVs OpInfo{..} SenderFunctionConf{..} clause = do
    let f = tyVarType opCarrier

        fnName = mkName _senderFnName

        funSig =
            SigD
                fnName
                ( ForallT
                    (opTyVars ++ [opCarrier $> SpecifiedSpec] & alterFnSigTVs)
                    (sendCxt opDataType f : opCxt)
                    (arrowChain opParamTypes (f `AppT` opResultType))
                )

        funInline = PragmaD (InlineP fnName Inline FunLike AllPhases)

    funDef <- FunD fnName <$> sequence [clause f & lift]

    -- Put documents
    lift do
        effDoc <- getDoc $ DeclDoc opName
        _senderFnDoc effDoc >>= mapM_ \doc -> do
            addModFinalizer $ putDoc (DeclDoc fnName) doc

        forM [0 .. length opParamTypes - 1] \i -> do
            argDoc <- getDoc $ ArgDoc opName i
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

reifyEffect :: Name -> Q (Info, DataInfo, EffectInfo)
reifyEffect name = do
    info <- reify name

    dataInfo <-
        analyzeData info
            & maybe (fail $ "Not datatype: ‘" <> pprint name <> "’") pure

    effClsInfo <-
        analyzeEffect dataInfo
            & either (fail . T.unpack) pure

    pure (info, dataInfo, effClsInfo)

analyzeEffect :: DataInfo -> Either T.Text EffectInfo
analyzeEffect DataInfo{..} = do
    (initTyVars, resultType) <- unsnoc dataTyVars & maybeToEither "No result type variable."
    (paramVars, carrier) <- unsnoc initTyVars & maybeToEither "No carrier type variable."

    let analyzeOp :: ConInfo -> Validation [T.Text] OpInfo
        analyzeOp ConInfo{..} = eitherToValidation do
            (opDataType, opCarrier, opResultType) <-
                maybe
                    ( pure
                        ( foldl' AppT (VarT dataName) (map tyVarType paramVars)
                        , carrier
                        , tyVarType resultType
                        )
                    )
                    decomposeGadtReturnType
                    conGadtReturnType

            let removeCarrierTV :: [TyVarBndr a] -> [TyVarBndr a]
                removeCarrierTV = filter ((tyVarName opCarrier /=) . tyVarName)

                opTyVars =
                    if isJust conGadtReturnType
                        then removeCarrierTV conTyVars
                        else map (SpecifiedSpec <$) (removeCarrierTV paramVars) ++ conTyVars

                opParamTypes = map snd conArgs

            Right
                OpInfo
                    { opName = conName
                    , opParamTypes = opParamTypes
                    , opDataType = opDataType
                    , opResultType = opResultType
                    , opTyVars = opTyVars
                    , opCarrier = opCarrier
                    , opCxt = conCxt
                    , opOrder =
                        if any (tyVarName opCarrier `occurs`) opParamTypes
                            then HigherOrder
                            else FirstOrder
                    }
          where
            decomposeGadtReturnType
                :: TH.Type -> Either [T.Text] (TH.Type, TyVarBndr (), TH.Type)
            decomposeGadtReturnType =
                unkindType >>> \case
                    e `AppT` SigT (VarT f) kf `AppT` x ->
                        Right (e, KindedTV f () kf, x)
                    e `AppT` VarT f `AppT` x ->
                        Right (e, PlainTV f (), x)
                    t ->
                        Left
                            [ "Unexpected form of GADT return type for the higher-order operation ‘"
                                <> T.pack (nameBase conName)
                                <> "’: "
                                <> T.pack (pprint t)
                            ]

    effCons <-
        traverse analyzeOp dataCons
            & validationToEither
            & mapLeft T.unlines

    pure
        EffectInfo
            { eName = dataName
            , eParamVars = paramVars
            , eCarrier = carrier
            , eOps = effCons
            }

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
