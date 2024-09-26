{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <=<" #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{-  The code before modification is licensed under the BSD3 License as
    shown in [1].  The modified code, in its entirety, is licensed under
    MPL 2.0. When redistributing, please ensure that you do not remove
    the BSD3 License text as indicated in [1].
    <https://github.com/pa-ba/compdata/blob/master/src/Data/Comp/Multi/Derive/HFunctor.hs>

    [1] Copyright (c) 2010--2011 Patrick Bahr, Tom Hvitved

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
Copyright   :  (c) 2010-2011 Patrick Bahr, Tom Hvitved
               (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.HFunctor.TH.Internal where

import Control.Monad (replicateM, zipWithM)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.TH.Internal (
    ConInfo (ConInfo),
    DataInfo (DataInfo),
    conArgs,
    conGadtReturnType,
    conName,
    occurs,
    tyVarName,
    tyVarType,
    unkindType,
 )
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.List.Infinite (Infinite, prependList)
import Data.Text qualified as T
import Formatting (int, sformat, shown, stext, (%))
import Language.Haskell.TH (
    Body (NormalB),
    Clause (Clause),
    Dec (FunD, InstanceD, PragmaD),
    Exp (AppE, CaseE, ConE, LamE, TupE, VarE),
    Inline (Inline),
    Match (Match),
    Name,
    Pat (ConP, TupP, VarP),
    Phases (AllPhases),
    Pragma (InlineP),
    Q,
    Quote (..),
    RuleMatch (FunLike),
    TyVarBndr (PlainTV),
    Type (AppT, ArrowT, ConT, ForallT, SigT, TupleT, VarT),
    appE,
    nameBase,
    pprint,
 )
import Language.Haskell.TH qualified as TH

{- |
Derive an instance of t'Data.Effect.HFunctor.HFunctor' for a type constructor of any higher-order
kind taking at least two arguments.
-}
deriveHFunctor :: (Infinite (Q TH.Type) -> Q TH.Type) -> DataInfo -> Q [Dec]
deriveHFunctor manualCxt (DataInfo _ name args cons) = do
    mapFnName <- newName "_f"
    let mapFn = VarE mapFnName

        initArgs = init args
        hfArgs = init initArgs

        hfArgNames = map (VarT . tyVarName) hfArgs

        -- The algorithm is based on: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/derive-functor
        hfmapClause :: ConInfo -> Q Clause
        hfmapClause ConInfo{..} = do
            let f = case conGadtReturnType of
                    Nothing -> last initArgs
                    Just t -> case t of
                        _ `AppT` VarT n `AppT` _ -> PlainTV n ()
                        _ `AppT` (VarT n `SigT` _) `AppT` _ -> PlainTV n ()
                        _ -> error $ "Encounted unknown structure: " ++ pprint t

                hfmapE :: TH.Type -> Exp -> Q Exp
                hfmapE tk
                    | fNotOccurs t = pure
                    | otherwise = \x -> case t of
                        VarT n `AppT` a | n == tyVarName f && fNotOccurs a -> pure $ mapFn `AppE` x
                        ArrowT `AppT` c `AppT` d ->
                            wrapLam \y -> hfmapE d . (x `AppE`) =<< cohfmapE c y
                        g `AppT` a
                            | fNotOccurs g ->
                                ((VarE 'fmap `AppE`) <$> wrapLam (hfmapE a)) <&> (`AppE` x)
                        ff `AppT` g `AppT` a
                            | fNotOccurs ff && fNotOccurs a ->
                                ((VarE 'hfmap `AppE`) <$> wrapLam (hfmapE $ g `AppT` a)) <&> (`AppE` x)
                        -- todo: tuple support
                        ForallT _ _ a -> hfmapE a x
                        _ ->
                            case mapTupleE hfmapE t x of
                                Just e -> e
                                Nothing -> fail $ "Encounted unsupported structure: " ++ pprint t
                  where
                    t = unkindType tk

                cohfmapE :: TH.Type -> Exp -> Q Exp
                cohfmapE tk
                    | not $ tyVarName f `occurs` t = pure
                    | otherwise = \x -> case t of
                        VarT n `AppT` a
                            | n == tyVarName f && fNotOccurs a ->
                                fail $ "Functor type variable occurs in contravariant position: " ++ pprint t
                        ArrowT `AppT` c `AppT` d ->
                            wrapLam \y -> cohfmapE d . (x `AppE`) =<< hfmapE c y
                        g `AppT` a
                            | fNotOccurs g ->
                                ((VarE 'fmap `AppE`) <$> wrapLam (cohfmapE a)) <&> (`AppE` x)
                        ff `AppT` _ `AppT` a
                            | fNotOccurs ff && fNotOccurs a ->
                                fail $ "Functor type variable occurs in contravariant position: " ++ pprint t
                        ForallT _ _ b' -> cohfmapE b' x
                        _ ->
                            case mapTupleE cohfmapE t x of
                                Just e -> e
                                Nothing -> fail $ "Encounted unsupported structure: " ++ pprint t
                  where
                    t = unkindType tk

                fNotOccurs = not . (tyVarName f `occurs`)

            vars <- replicateM (length conArgs) (newName "x")
            mappedArgs <- zipWithM hfmapE (map snd conArgs) (map VarE vars)
            let body = foldl' AppE (ConE conName) mappedArgs
            pure $ Clause [VarP mapFnName, ConP conName [] (map VarP vars)] (NormalB body) []

    cxt <-
        manualCxt $
            map (pure . tyVarType) hfArgs
                `prependList` error
                    ( T.unpack $
                        sformat
                            ( "Too many data type arguments in use. The number of usable type arguments in the data type ‘"
                                % shown
                                % "’ to be derived is "
                                % int
                                % ". ("
                                % stext
                                % ")"
                            )
                            name
                            (length hfArgs)
                            (T.intercalate ", " $ map ((\t -> "‘" <> t <> "’") . T.pack . nameBase . tyVarName) hfArgs)
                    )

    hfmapDecls <- FunD 'hfmap <$> mapM hfmapClause cons
    let fnInline = PragmaD (InlineP 'hfmap Inline FunLike AllPhases)

    pure
        [ InstanceD
            Nothing
            [cxt]
            (ConT ''HFunctor `AppT` foldl' AppT (ConT name) hfArgNames)
            [hfmapDecls, fnInline]
        ]

wrapLam :: (Exp -> Q Exp) -> Q Exp
wrapLam f = do
    x <- newName "x"
    LamE [VarP x] <$> f (VarE x)

mapTupleE :: (TH.Type -> Exp -> Q Exp) -> TH.Type -> Exp -> Maybe (Q Exp)
mapTupleE f t e = do
    es <- decomposeTupleT t
    let n = length es
    Just do
        xs <- newNames n "x"
        ys <- zipWithM f es $ map VarE xs
        pure $ CaseE e [Match (TupP $ map VarP xs) (NormalB $ TupE $ map Just ys) []]

decomposeTupleT :: TH.Type -> Maybe [TH.Type]
decomposeTupleT = go [] 0
  where
    go :: [TH.Type] -> Int -> TH.Type -> Maybe [TH.Type]
    go acc !n = \case
        TupleT m | m == n -> Just acc
        f `AppT` a -> go (a : acc) (n + 1) f
        _ -> Nothing
{-# INLINE decomposeTupleT #-}

-- * Utility functions

{- |
This function provides a list (of the given length) of new names based
on the given string.
-}
newNames :: Int -> String -> Q [Name]
newNames n name = replicateM n (newName name)

iter :: (Eq t, Num t, Quote m) => t -> m Exp -> m Exp -> m Exp
iter 0 _ e = e
iter n f e = iter (n - 1) f (f `appE` e)
