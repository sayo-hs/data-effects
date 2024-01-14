{-# LANGUAGE TemplateHaskellQuotes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

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

{- |
Copyright   :  (c) 2020 Michael Szvetits
               (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.Class.TH.Internal where

import Control.Monad (forM, replicateM, unless, when)
import Control.Monad.IO.Class (MonadIO)
import Data.List (intercalate, nub)
import Language.Haskell.TH.Lib (
    appT,
    conT,
    patSynSigD,
    sigT,
    varT,
 )
import Language.Haskell.TH.Syntax (
    Con,
    Cxt,
    Dec (ClassD, SigD),
    Info (ClassI),
    Kind,
    Name,
    Q,
    Quote (newName),
    TyVarBndr (KindedTV, PlainTV),
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
        StarT,
        UInfixT,
        VarT
    ),
    nameBase,
    reify,
 )

import Control.Effect.Class (LiftIns (LiftIns))
import Control.Lens ((%~), (^?), _head, _last)
import Control.Monad.Writer (Any (Any), runWriterT, tell)
import Data.Bool (bool)
import Data.Char (toUpper)
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Extra (dropEnd)
import Data.Maybe (isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Haskell.TH (
    Bang (Bang),
    Con (ForallC, GadtC),
    Dec (DataD),
    DerivClause,
    FunDep (FunDep),
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    Specificity (SpecifiedSpec),
    arrowT,
    conP,
    implBidir,
    mkName,
    patSynD,
    pragCompleteD,
    prefixPatSyn,
    tySynD,
    varP,
 )
import Language.Haskell.TH.Datatype (freeVariables)

-- | Generate /instruction/ and /signature/ data types from an effect class, from 'EffectInfo'.
generateEffectDataByEffInfo ::
    -- | An effect order of an effect data type to generate.
    EffectOrder ->
    -- | A name of an effect data type to generate.
    Name ->
    EffectInfo ->
    Q (DataInfo (), Dec)
generateEffectDataByEffInfo order effDataName info = do
    effDataInfo <- do
        let pvs = fst <$> effParamVars info

        additionalTypeParams <- do
            a <- do
                a <- newName "a"
                pure $ KindedTV a () StarT

            pure case order of
                FirstOrder -> [a]
                HigherOrder -> [unkindTyVar $ effMonad info, a]

        cons <- do
            (errorMethods, cons) <- do
                consWithMethodInfo <- do
                    effData <- do
                        let paramTypes = fmap (tyVarType . unkindTyVar) pvs
                        foldl appT (conT effDataName) paramTypes

                    forM (effMethods info) \method ->
                        (methodName method,)
                            <$> interfaceToCon info effData method

                pure . partitionEithers $
                    consWithMethodInfo <&> \(methodName, (methodOrder, con)) ->
                        if methodOrder == order
                            then Right con
                            else Left (methodOrder, nameBase methodName)

            unless (null errorMethods) $
                fail $
                    "Unexpected order of effect methods: "
                        <> intercalate
                            ", "
                            ( errorMethods <&> \(methodOrder, name) ->
                                name <> " [" <> [fst $ effectOrderSymbol methodOrder] <> "]"
                            )

            pure cons

        pure $ DataInfo [] effDataName (pvs ++ additionalTypeParams) cons []

    pure (effDataInfo, infoToDataD effDataInfo)

-- | A reified information of a datatype.
data DataInfo flag = DataInfo
    { dataCxt :: Cxt
    , dataName :: Name
    , dataTyVars :: [TyVarBndr flag]
    , dataCons :: [Con]
    , dataDerivings :: [DerivClause]
    }

-- | Convert the reified information of the datatype to a definition.
infoToDataD :: DataInfo () -> Dec
infoToDataD (DataInfo cxt name args cons deriv) = DataD cxt name args Nothing cons deriv

-- | Convert an effect method interface to a constructor of the effect data type.
interfaceToCon ::
    EffectInfo ->
    Type ->
    MethodInterface ->
    Q (EffectOrder, Con)
interfaceToCon info effData MethodInterface{..} =
    (methodOrder,) <$> do
        effDataFunctor <- case methodOrder of
            FirstOrder -> pure effData
            HigherOrder -> pure effData `appT` (unkindType <$> tyVarType (effMonad info))

        let vars =
                foldl
                    (\acc t -> nub $ acc ++ freeVariables t)
                    (tyVarName . fst <$> effParamVars info)
                    (methodParamTypes ++ [methodReturnType])

        pure $
            ForallC ((`PlainTV` SpecifiedSpec) <$> vars) methodCxt $
                GadtC
                    [renameMethodToCon methodName]
                    (methodParamTypes & map (Bang NoSourceUnpackedness NoSourceStrictness,))
                    (AppT effDataFunctor methodReturnType)

{- |
Decompose an effect method interface type to get the effect order, the list of argument types, and
the return type.
-}
analyzeMethodInterface :: TyVarBndr () -> Type -> Q (EffectOrder, [Type], Type, Cxt)
analyzeMethodInterface m interface = do
    ((resultType, cxt, paramTypes), Any isHigherOrderMethod) <- runWriterT $ go interface
    pure (bool FirstOrder HigherOrder isHigherOrderMethod, paramTypes, resultType, cxt)
  where
    go = \case
        ArrowT `AppT` l `AppT` r -> do
            when (tyVarName m `occurs` l) $ tell $ Any True
            fmap (l :) <$> go r
        ForallT _ cxt u -> do
            (r, c, p) <- go u
            return (r, cxt ++ c, p)
        VarT n `AppT` a | n == tyVarName m -> pure (a, [], [])
        other -> fail $ "Expected a pure type of the form 'm a', but encountered: " ++ show other

-- | Convert a lower-camel-cased method name to an upper-camel-cased constructor name.
renameMethodToCon :: Name -> Name
renameMethodToCon = mkName . (_head %~ toUpper) . nameBase

-- | An order of effect.
data EffectOrder = FirstOrder | HigherOrder
    deriving (Show, Eq, Ord)

-- | Is the order of effect higher-order?
isHigherOrder :: EffectOrder -> Bool
isHigherOrder = \case
    FirstOrder -> False
    HigherOrder -> True

{- |
The default naming convention of effect data types.

Add an @I@ or @S@ symbol indicating the order of the effect to the end of the effect class name.

If the name of the effect class ends in @F@ or @H@, depending on its order, replace @F@ or @H@ with
@I@ or @S@.
-}
defaultEffectDataNamer :: EffectOrder -> String -> String
defaultEffectDataNamer order clsName =
    effNameBase ++ [dataOrderSym]
  where
    (clsOrderSym, dataOrderSym) = effectOrderSymbol order
    effNameBase =
        if clsName ^? _last == Just clsOrderSym
            then dropEnd 1 clsName
            else clsName

-- | Symbol letters representing the order of the effect.
effectOrderSymbol :: EffectOrder -> (Char, Char)
effectOrderSymbol = \case
    FirstOrder -> ('F', 'I')
    HigherOrder -> ('H', 'S')

-- ** Generating Synonyms about LiftIns

{- |
Generate the pattern synonyms for instruction constructors:

    @pattern BazS ... = LiftIns (Baz ...)@
-}
generateLiftInsPatternSynonyms :: Name -> EffectInfo -> Q [Dec]
generateLiftInsPatternSynonyms dataName info = do
    patSyns <-
        forM (effMethods info) \MethodInterface{..} -> do
            let conName = renameMethodToCon methodName
                newConName = mkName $ nameBase conName ++ "S"
            args <- replicateM (length methodParamTypes) (newName "x")
            a <- varT . mkName . show <$> newName "a"
            (newConName,)
                <$> sequence
                    [ patSynSigD
                        newConName
                        -- For some reason, if I don't write constraints in this form, the type is
                        -- not inferred properly (why?).
                        [t|
                            () =>
                            ($a ~ $(pure methodReturnType)) =>
                            $( foldr
                                (\l r -> arrowT `appT` pure l `appT` r)
                                [t|
                                    $(liftInsType dataName $ tyVarName . fst <$> effParamVars info)
                                        $(varT $ tyVarName $ effMonad info)
                                        $a
                                    |]
                                methodParamTypes
                             )
                            |]
                    , patSynD
                        newConName
                        (prefixPatSyn args)
                        implBidir
                        (conP 'LiftIns [conP conName $ varP <$> args])
                    ]

    (concatMap snd patSyns ++)
        <$> sequence [pragCompleteD (fst <$> patSyns) Nothing]

{- |
Generate the type synonym for an instruction datatype:

    @type (FoobarFS ...) = LiftIns (FoobarI ...)@
-}
generateLiftInsTypeSynonym :: EffectInfo -> Name -> Q Dec
generateLiftInsTypeSynonym info dataName = do
    nameS <- mkName <$> renameI2FS (nameBase dataName)
    tySynD
        nameS
        (pvs <&> (`PlainTV` ()))
        (liftInsType dataName pvs)
  where
    pvs = tyVarName . fst <$> effParamVars info

renameI2FS :: String -> Q String
renameI2FS name = dropEndI name <&> (++ "FS")

dropEndI :: String -> Q String
dropEndI name =
    if name ^? _last == Just 'I'
        then pure $ dropEnd 1 name
        else fail $ "The name doesn't end in 'I': \"" <> name <> "\"."

liftInsType :: Name -> [Name] -> Q Type
liftInsType dataName pvs =
    conT ''LiftIns `appT` foldl appT (conT dataName) (varT <$> pvs)

applyEffPVs :: Name -> [TyVarBndr ()] -> Q Type
applyEffPVs effClsName = foldl appT (conT effClsName) . fmap tyVarType

-- ** Reification of Effect Class

-- | Information about effect type classes.
data EffectInfo = EffectInfo
    { effCxts :: [Type]
    , effName :: Name
    , effParamVars :: [(TyVarBndr (), IsDepParam)]
    , effMonad :: TyVarBndr ()
    , effMethods :: [MethodInterface]
    }

newtype IsDepParam = IsDepParam {isDepParam :: Bool}
    deriving stock (Eq, Show)

effParamVar :: (Name, Maybe Kind) -> TyVarBndr ()
effParamVar (n, k) = case k of
    Just k' -> KindedTV n () k'
    Nothing -> PlainTV n ()

data MethodInterface = MethodInterface
    { methodName :: Name
    , methodOrder :: EffectOrder
    , methodParamTypes :: [Type]
    , methodReturnType :: Type
    , methodCxt :: Cxt
    }

-- | Given a type class name, extracts infos about an effect.
reifyEffectInfo :: Name -> Q EffectInfo
reifyEffectInfo className = do
    info <- reify className
    case info of
        ClassI (ClassD cxts name tyVars funDeps decs) _ -> do
            (paramVars, carrier) <-
                case tyVars of
                    [] ->
                        fail $
                            "The specified effect type class `"
                                ++ nameBase name
                                ++ "' has no monad type variable. "
                                ++ "It is expected to be the last type variable."
                    vs -> pure (init vs, last vs)

            depParams <- scanFunDeps paramVars carrier funDeps
            let paramVars' =
                    paramVars <&> \pv ->
                        (pv, IsDepParam $ tyVarName pv `Set.member` depParams)

            EffectInfo cxts name paramVars' carrier
                <$> sequence
                    [ do
                        (order, paramTypes, retType, cxt) <- analyzeMethodInterface carrier t
                        pure $ MethodInterface n order paramTypes retType cxt
                    | SigD n t <- decs
                    ]
        other ->
            fail $
                "The specified name `"
                    ++ nameBase className
                    ++ "' is not a type class, but the following instead: "
                    ++ show other

scanFunDeps :: [TyVarBndr ()] -> TyVarBndr () -> [FunDep] -> Q (Set Name)
scanFunDeps paramVars carrier = \case
    [] -> pure Set.empty
    [FunDep lhs rhs]
        | carrier' `Set.member` lhs'
            && Set.delete carrier' lhs' `Set.union` rhs' == paramVars'
            && Set.disjoint lhs' rhs' ->
            pure rhs'
      where
        lhs' = Set.fromList lhs
        rhs' = Set.fromList rhs
        paramVars' = Set.fromList $ tyVarName <$> paramVars
        carrier' = tyVarName carrier
    fds ->
        fail $
            "Unsupported form of functional dependencies: "
                <> intercalate
                    ", "
                    ( fds <&> \(FunDep lhs rhs) ->
                        unwords (show <$> lhs) <> " -> " <> unwords (show <$> rhs)
                    )

-- | Constructs the type of an effect, i.e. the type class without its monad parameter.
effectType :: EffectInfo -> Q Type
effectType info =
    foldl
        appT
        (conT $ effName info)
        (fmap tyVarType (fst <$> effParamVars info))

partitionSuperEffects :: EffectInfo -> (Cxt, [Type])
partitionSuperEffects info =
    ( filter (isNothing . extract) cxts
    , mapMaybe extract (effCxts info)
    )
  where
    cxts = effCxts info
    m = tyVarName (effMonad info)
    extract = \case
        ForallT _ _ t -> extract t
        SigT t _ -> extract t
        ParensT t -> extract t
        t `AppT` VarT n | n == m -> Just t
        InfixT t _ (VarT n) | n == m -> Just t
        UInfixT t _ (VarT n) | n == m -> Just t
        AppKindT t _ -> extract t
        ImplicitParamT _ t -> extract t
        _ -> Nothing

{- |
Extracts the super classes of an effect which have the kind of effects. As an example, for the
following effect ...

@class (State s m, Monad m) => MyEffect s m where ...@

... this would pure [State s, Monad].
-}
superEffects :: EffectInfo -> [Type]
superEffects = snd . partitionSuperEffects

{- |
Like superEffects, but ignores super classes from base (i.e., Applicative, Functor, Monad, MonadIO).
-}
superEffectsWithoutBase :: EffectInfo -> [Type]
superEffectsWithoutBase =
    filter (not . isBase) . superEffects
  where
    isBase = \case
        ConT n -> n `elem` [''Applicative, ''Functor, ''Monad, ''MonadIO]
        _ -> False

effectParamCxt :: EffectInfo -> Cxt
effectParamCxt = fst . partitionSuperEffects

-- ** Utility functions

-- | Construct a namer from a conversion function of string.
pureNamer :: (String -> String) -> Name -> Q Name
pureNamer f = pure . mkName . f . nameBase

-- | Throws away all kind information from a type.
unkindType :: Type -> Type
unkindType = \case
    ForallT vs ps t -> ForallT (fmap unkindTyVar vs) (fmap unkindType ps) (unkindType t)
    AppT l r -> AppT (unkindType l) (unkindType r)
    SigT t _ -> t
    InfixT l n r -> InfixT (unkindType l) n (unkindType r)
    UInfixT l n r -> UInfixT (unkindType l) n (unkindType r)
    ParensT t -> ParensT (unkindType t)
    AppKindT t _ -> unkindType t
    ImplicitParamT s t -> ImplicitParamT s (unkindType t)
    other -> other

-- | Throws away the kind information of a type variable.
unkindTyVar :: TyVarBndr a -> TyVarBndr a
unkindTyVar (KindedTV n s _) = PlainTV n s
unkindTyVar unkinded = unkinded

-- | Converts a type variable to a type.
tyVarType :: TyVarBndr a -> Q Type
tyVarType (PlainTV n _) = varT n
tyVarType (KindedTV n _ k) = sigT (varT n) k

tyVarKind :: TyVarBndr a -> Q Type
tyVarKind (KindedTV _ _ k) = pure k
tyVarKind (PlainTV _ _) = fail "The type variable has no kind."

-- | pures the name of a type variable.
tyVarName :: TyVarBndr a -> Name
tyVarName (PlainTV n _) = n
tyVarName (KindedTV n _ _) = n

-- | Counts the parameters of a type.
paramCount :: Type -> Int
paramCount = \case
    ArrowT `AppT` _ `AppT` r -> 1 + paramCount r
    ForallT _ _ t -> paramCount t
    _ -> 0

-- | Checks if a name m appears somewhere in a type.
occurs :: Name -> Type -> Bool
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
