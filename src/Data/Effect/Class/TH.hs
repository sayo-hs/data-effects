{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

-- The code before modification is BSD3 licensed, (c) 2020 Michael Szvetits.

{- |
Copyright   :  (c) 2020 Michael Szvetits
               (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides @TemplateHaskell@ functions to generate the effect data types
(/instruction/s and /signature/s) for effect type classes.
-}
module Data.Effect.Class.TH where

import Control.Monad (forM, forM_, guard, unless, when)
import Control.Monad.IO.Class (MonadIO)
import Data.List (intercalate, partition)
import Data.Maybe (isNothing, mapMaybe)

import Language.Haskell.TH.Lib (
    appT,
    conT,
    sigT,
    varT,
 )
import Language.Haskell.TH.Syntax (
    Con,
    Cxt,
    Dec (ClassD, SigD),
    FunDep (..),
    Info (ClassI),
    Kind,
    Name,
    Q,
    Quote (newName),
    TyVarBndr (..),
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

import Data.Set qualified as Set

import Control.Effect.Class.TH.HFunctor (DataInfo (DataInfo), infoToDataD, tyVarName)
import Control.Lens (bimap, (%~), _head)
import Control.Monad.Writer (Any (Any), execWriterT, runWriterT, tell)
import Data.Bool (bool)
import Data.Char (toUpper)
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Monoid (Last (Last))
import Language.Haskell.TH (
    Bang (Bang),
    Con (GadtC),
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    mkName,
 )

-- | Generate /instruction/ and /signature/ data types from the effect class of the given name.
makeEffectData :: EffectDataNamer -> MakeEmptyEffectData -> Name -> Q [Dec]
makeEffectData effDataNamer makeEmptyEffData effClsName =
    fmap snd . generateEffectDataByEffInfo effDataNamer makeEmptyEffData =<< reifyEffectInfo effClsName

-- | Generate only an /instruction/ data type from the effect class of the given name.
makeEffectDataF :: EffectDataNamer -> MakeEmptyEffectData -> Name -> Q [Dec]
makeEffectDataF = makeEffectData . guardEffDataNamer FirstOrder

-- | Generate only a /signature/ data type from the effect class of the given name.
makeEffectDataH :: EffectDataNamer -> MakeEmptyEffectData -> Name -> Q [Dec]
makeEffectDataH = makeEffectData . guardEffDataNamer HigherOrder

{-
{- |
A restriction on the order of effects.

If methods are found to be trapped by the restriction, an error will occur during effect data type
generation.
-}
data OrderRestriction
    = Only EffectOrder
    | NoOrderRestriction

-- | Apply a restriction to the namer.
restrictNamer :: OrderRestriction -> EffectDataNamer -> EffectDataNamer
restrictNamer = \case
    Only order -> guardEffDataNamer order
    NoOrderRestriction -> id

restrictOnlyFirstOrder :: EffectDataNamer -> EffectDataNamer
restrictOnlyFirstOrder = guardEffDataNamer FirstOrder

restrictOnlyHigherOrder :: EffectDataNamer -> EffectDataNamer
restrictOnlyHigherOrder = guardEffDataNamer FirstOrder
-}

-- | A configuration of whether to generate an effect data type even when the one is empty.
data MakeEmptyEffectData
    = -- | Generate an effect data type even when the one is empty.
      MakeEffectDataEvenIfEmpty
    | -- | Doesn't generate an effect data type when the one is empty.
      NoMakeEmptyEffectData
    deriving (Eq)

-- | Generate /instruction/ and /signature/ data types from an effect class, from @EffectInfo@.
generateEffectDataByEffInfo ::
    EffectDataNamer ->
    MakeEmptyEffectData ->
    EffectInfo ->
    Q (EffDataInfo, [Dec])
generateEffectDataByEffInfo effDataNamer makeEmptyEffData info = do
    let pvs = effParamVars info

    nameF <- effDataNamer FirstOrder $ effName info
    nameH <- effDataNamer HigherOrder $ effName info

    (consH, consF) <- do
        let paramTypes = fmap (tyVarType . unkindTyVar) pvs
            applyParamTypes effDataName = foldl appT (conT effDataName) paramTypes
        insType <- mapM applyParamTypes nameF
        sigType <- mapM applyParamTypes nameH

        consM <-
            forM (effMethods info) \method ->
                (methodName method,)
                    <$> interfaceToCon info (bool insType sigType . isHigherOrder) method

        let (errorMethods, cons) =
                partitionEithers $
                    consM <&> \(methodName, (order, conM)) ->
                        case conM of
                            Nothing -> Left (order, nameBase methodName)
                            Just con -> Right (order, con)

        unless (null errorMethods) $
            fail $
                "Unexpected order of effect methods: "
                    <> intercalate
                        ", "
                        ( errorMethods <&> \(order, name) ->
                            name <> " [" <> [effectOrderSymbol order] <> "]"
                        )

        pure $ bimap (snd <$>) (snd <$>) $ partition (isHigherOrder . fst) cons

    a <- do
        a <- newName "a"
        pure $ KindedTV a () StarT

    let makeEffData nameM additionalTypeParams cons order =
            forM_ nameM \name ->
                when (makeEmptyEffData == MakeEffectDataEvenIfEmpty || not (null cons)) $
                    let dataInfo = DataInfo [] name (pvs ++ additionalTypeParams) cons []
                     in tell (effDataGenResult dataInfo order, [infoToDataD dataInfo])

    execWriterT do
        makeEffData nameF [a] consF FirstOrder
        makeEffData nameH [unkindTyVar $ effMonad info, a] consH HigherOrder
        pure ()

newtype EffDataInfo = EffDataInfo
    {getEffDataInfo :: (Maybe (DataInfo ()), Maybe (DataInfo ()))}
    deriving (Semigroup, Monoid) via (Last (DataInfo ()), Last (DataInfo ()))

effDataGenResult :: DataInfo () -> EffectOrder -> EffDataInfo
effDataGenResult dInfo = \case
    FirstOrder -> EffDataInfo (Just dInfo, Nothing)
    HigherOrder -> EffDataInfo (Nothing, Just dInfo)

getEffDataInfoOn :: EffectOrder -> EffDataInfo -> Maybe (DataInfo ())
getEffDataInfoOn =
    (. getEffDataInfo) . \case
        FirstOrder -> fst
        HigherOrder -> snd

-- | Convert an effect method interface to a constructor of the effect data type.
interfaceToCon ::
    EffectInfo ->
    (EffectOrder -> Maybe Type) ->
    MethodInterface ->
    Q (EffectOrder, Maybe Con)
interfaceToCon info toEffData MethodInterface{..} =
    (methodOrder,) <$> forM (toEffData methodOrder) \effData -> do
        effData' <- case methodOrder of
            FirstOrder -> pure effData
            HigherOrder -> pure effData `appT` (unkindType <$> tyVarType (effMonad info))
        pure $
            GadtC
                [renameMethodToCon methodName]
                (methodParamTypes & map (Bang NoSourceUnpackedness NoSourceStrictness,))
                (AppT effData' methodReturnType)

{- |
Decompose an effect method interface type to get the effect order, the list
of argument types, and the return type.
-}
analyzeMethodInterface :: TyVarBndr () -> Type -> Q (EffectOrder, [Type], Type)
analyzeMethodInterface m interface = do
    ((resultType, paramTypes), Any isHigherOrderMethod) <- runWriterT $ go interface
    pure (bool FirstOrder HigherOrder isHigherOrderMethod, paramTypes, resultType)
  where
    go = \case
        ArrowT `AppT` l `AppT` r -> do
            when (tyVarName m `occurs` l) $ tell $ Any True
            fmap (l :) <$> go r
        ForallT _ _ u -> go u
        VarT n `AppT` a | n == tyVarName m -> pure (a, [])
        other -> fail $ "Expected a pure type of the form 'm a', but encountered: " ++ show other

-- | Convert a lower-camel-cased method name to an upper-camel-cased constructor name.
renameMethodToCon :: Name -> Name
renameMethodToCon = mkName . (_head %~ toUpper) . nameBase

-- | A naming convention of effect data types.
type EffectDataNamer = EffectOrder -> Name -> Q (Maybe Name)

{- |
A default naming convention of effect data types.

Add an @F@ or @H@ symbol indicating the order of the effect to the end of the effect class name.
-}
defaultEffectDataNamer :: EffectDataNamer
defaultEffectDataNamer order = namer (++ [effectOrderSymbol order])

{- |
Restrict an effect data type namer to allow only effect methods of the specified effect order.

Add an @F@ or @H@ symbol indicating the order of the effect to the end of the effect class name.
-}
guardEffDataNamer :: EffectOrder -> EffectDataNamer -> EffectDataNamer
guardEffDataNamer o1 effDataNamer o2 name =
    (guard (o1 == o2) *>) <$> effDataNamer o2 name

-- | An order of effect.
data EffectOrder = FirstOrder | HigherOrder
    deriving (Show, Eq, Ord)

-- | Is the order of effect higher-order?
isHigherOrder :: EffectOrder -> Bool
isHigherOrder = \case
    FirstOrder -> False
    HigherOrder -> True

-- | The letter of the symbol of the order of effect.
effectOrderSymbol :: EffectOrder -> Char
effectOrderSymbol = \case
    FirstOrder -> 'F'
    HigherOrder -> 'H'

-- | Information about effect type classes.
data EffectInfo = EffectInfo
    { effCxts :: [Type]
    , effName :: Name
    , effParams :: [(Name, EffectParamKind)]
    , effMonad :: TyVarBndr ()
    , effMethods :: [MethodInterface]
    }

data EffectParamKind
    = NonCharacteristicsParam (Maybe Kind)
    | CharacteristicsParam Kind

effParamVar :: (Name, EffectParamKind) -> TyVarBndr ()
effParamVar (n, k) = case k of
    NonCharacteristicsParam k' -> case k' of
        Just k'' -> KindedTV n () k''
        Nothing -> PlainTV n ()
    CharacteristicsParam k' -> KindedTV n () k'

effParamVars :: EffectInfo -> [TyVarBndr ()]
effParamVars = map effParamVar . effParams

data MethodInterface = MethodInterface
    { methodName :: Name
    , methodOrder :: EffectOrder
    , methodParamTypes :: [Type]
    , methodReturnType :: Type
    }

-- | Given a type class name, extracts infos about an effect.
reifyEffectInfo :: Name -> Q EffectInfo
reifyEffectInfo className = do
    info <- reify className
    case info of
        ClassI (ClassD cxts name tyVars funDeps decs) _ -> do
            (paramVars_, monad) <-
                case tyVars of
                    [] ->
                        fail $
                            "The specified effect type class `"
                                ++ nameBase name
                                ++ "' has no monad type variable. "
                                ++ "It is expected to be the last type variable."
                    vs -> pure (init vs, last vs)
            let characteristicsParams_ =
                    foldr
                        (Set.intersection . Set.fromList . funDepLhs)
                        (Set.fromList $ map tyVarName paramVars_)
                        funDeps
            params <-
                forM paramVars_ \pv ->
                    if Set.member (tyVarName pv) characteristicsParams_
                        then case pv of
                            KindedTV n _ k -> pure (n, CharacteristicsParam k)
                            PlainTV n _ ->
                                (n,) . CharacteristicsParam . VarT
                                    <$> newName ("k_" ++ nameBase n)
                        else pure $ mkNonCharacteristicsParam pv
            EffectInfo cxts name params monad
                <$> sequence
                    [ do
                        (order, paramTypes, retType) <- analyzeMethodInterface monad t
                        pure $ MethodInterface n order paramTypes retType
                    | SigD n t <- decs
                    ]
        other ->
            fail $
                "The specified name `"
                    ++ nameBase className
                    ++ "' is not a type class, but the following instead: "
                    ++ show other

funDepLhs :: FunDep -> [Name]
funDepLhs (FunDep lhs _) = lhs

mkNonCharacteristicsParam :: TyVarBndr a -> (Name, EffectParamKind)
mkNonCharacteristicsParam = \case
    KindedTV n _ k -> (n, NonCharacteristicsParam $ Just k)
    PlainTV n _ -> (n, NonCharacteristicsParam Nothing)

-- | Constructs the type of an effect, i.e. the type class without its monad parameter.
effectType :: EffectInfo -> Q Type
effectType info =
    foldl
        appT
        (conT $ effName info)
        (fmap tyVarType (effParamVars info))

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

-- * Utility functions

-- | Construct a namer from a conversion function of string.
namer :: (String -> String) -> Name -> Q (Maybe Name)
namer f = pure . Just . mkName . f . nameBase

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

-- | Counts the parameters of a type.
paramCount :: Type -> Int
paramCount = \case
    ArrowT `AppT` _ `AppT` r -> 1 + paramCount r
    ForallT _ _ t -> paramCount t
    _ -> 0

-- | Checks if a name m appears somewhere in a type.
occurs :: Name -> Type -> Bool
occurs m = \case
    ForallT _ _ t -> m `occurs` t
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
