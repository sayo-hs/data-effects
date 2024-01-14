{-# LANGUAGE TemplateHaskellQuotes #-}

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

{-
This fork was made to work around the problem that the
'Control.Effect.Class.Machinery.TH.makeEffect' function that generates multiple
definitions at once for convenience is not possible with only the original
'Data.Comp.Multi.Derive.makeHFunctor' function due to TH limitations,
because the original function takes the name of the data type as an argument,
but there is no version that takes 'DataInfo' as an argument (the data type
reification and the HFunctor derivation process are not separated as functions).
-}

{- |
Copyright   :  (c) 2010-2011 Patrick Bahr, Tom Hvitved
               (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.Class.TH.HFunctor.Internal where

import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Monad (replicateM, (<=<))
import Data.Effect.Class.TH.Internal (DataInfo (DataInfo), tyVarName)
import Data.Maybe (catMaybes)
import Language.Haskell.TH (
    Body (NormalB),
    Clause (Clause),
    Con (ForallC, GadtC, InfixC, NormalC, RecC),
    Cxt,
    Dec (DataD, InstanceD, NewtypeD),
    Exp,
    Info (TyConI),
    Name,
    Pat (ConP, VarP, WildP),
    Q,
    Quote (..),
    Type (AppT, ConT, ForallT, SigT, VarT),
    appE,
    conE,
    funD,
    varE,
 )
import Language.Haskell.TH.Syntax (StrictType)

{- |
Derive an instance of 'HFunctor' for a type constructor of any higher-order kind taking at least two
arguments, from 'DataInfo'.
-}
deriveHFunctor :: DataInfo flag -> Q [Dec]
deriveHFunctor (DataInfo _cxt name args constrs _deriving) = do
    let args' = init args
        fArg = VarT . tyVarName $ last args'
        argNames = map (VarT . tyVarName) (init args')
        complType = foldl AppT (ConT name) argNames
        classType = AppT (ConT ''HFunctor) complType
    constrs' <- mapM (mkPatAndVars . isFarg fArg <=< normalConExp) constrs
    hfmapDecl <- funD 'hfmap (map hfmapClause constrs')
    return [mkInstanceD [] classType [hfmapDecl]]
  where
    isFarg fArg (constr, args_, ty) = (constr, map (`containsType'` getBinaryFArg fArg ty) args_)
    filterVar _ nonFarg [] x = nonFarg x
    filterVar farg _ [depth] x = farg depth x
    filterVar _ _ _ _ = error "functor variable occurring twice in argument type"
    filterVars args_ varNs farg nonFarg = zipWith (filterVar farg nonFarg) args_ varNs
    mkCPat constr varNs = ConP constr [] $ map mkPat varNs
    mkPat = VarP
    mkPatAndVars ::
        (Name, [[t]]) ->
        Q (Q Exp, Pat, (t -> Q Exp -> c) -> (Q Exp -> c) -> [c], Bool, [Q Exp], [(t, Name)])
    mkPatAndVars (constr, args_) =
        do
            varNs <- newNames (length args_) "x"
            return
                ( conE constr
                , mkCPat constr varNs
                , \f g -> filterVars args_ varNs (\d x -> f d (varE x)) (g . varE)
                , not (all null args_)
                , map varE varNs
                , catMaybes $ filterVars args_ varNs (curry Just) (const Nothing)
                )
    hfmapClause (con, pat, vars', hasFargs, _, _) =
        do
            fn <- newName "f"
            let f = varE fn
                fp = if hasFargs then VarP fn else WildP
                vars = vars' (\d x -> iter d [|fmap|] f `appE` x) id
            body <- foldl appE con vars
            return $ Clause [fp, pat] (NormalB body) []

{- |
This function abstracts away @newtype@ declaration, it turns them into
@data@ declarations.
-}
abstractNewtype :: Info -> Maybe (DataInfo ())
abstractNewtype = \case
    TyConI (NewtypeD cxt name args _ constr derive) -> Just (DataInfo cxt name args [constr] derive)
    TyConI (DataD cxt name args _ constrs derive) -> Just (DataInfo cxt name args constrs derive)
    _ -> Nothing

{- |
This function provides the name and the arity of the given data
constructor, and if it is a GADT also its type.
-}
normalCon :: Con -> (Name, [StrictType], Maybe Type)
normalCon (NormalC constr args) = (constr, args, Nothing)
normalCon (RecC constr args) = (constr, map (\(_, s, t) -> (s, t)) args, Nothing)
normalCon (InfixC a constr b) = (constr, [a, b], Nothing)
normalCon (ForallC _ _ constr) = normalCon constr
normalCon (GadtC (constr : _) args typ) = (constr, args, Just typ)
normalCon _ = error "missing case for 'normalCon'"

normalConExp :: Con -> Q (Name, [Type], Maybe Type)
normalConExp con = pure (n, map snd ts, t)
  where
    (n, ts, t) = normalCon con

containsType' :: Type -> Type -> [Int]
containsType' = run 0
  where
    run n s t
        | s == t = [n]
        | otherwise = case s of
            ForallT _ _ s' -> run n s' t
            -- only going through the right-hand side counts!
            AppT s1 s2 -> run n s1 t ++ run (n + 1) s2 t
            SigT s' _ -> run n s' t
            _ -> []

{- |
Auxiliary function to extract the first argument of a binary type
application (the second argument of this function). If the second
argument is @Nothing@ or not of the right shape, the first argument
is returned as a default.
-}
getBinaryFArg :: Type -> Maybe Type -> Type
getBinaryFArg _ (Just (AppT (AppT _ t) _)) = t
getBinaryFArg def _ = def

mkInstanceD :: Cxt -> Type -> [Dec] -> Dec
mkInstanceD = InstanceD Nothing

{- |
This function provides a list (of the given length) of new names based
on the given string.
-}
newNames :: Int -> String -> Q [Name]
newNames n name = replicateM n (newName name)

iter :: (Eq t, Num t, Quote m) => t -> m Exp -> m Exp -> m Exp
iter 0 _ e = e
iter n f e = iter (n - 1) f (f `appE` e)
