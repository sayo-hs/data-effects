{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect where

import Control.Alternative.Free qualified as Tree
import Control.Alternative.Free.Final qualified as Final
import Control.Applicative (Alternative)
import Control.Applicative.Free qualified as Tree
import Control.Applicative.Free.Fast qualified as Fast
import Control.Applicative.Free.Final qualified as Final
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Effect (Emb (Emb))
import Data.Effect.HFunctor (hfmap)
import Data.Effect.OpenUnion (Has, In, Index, KeyIndex, LabelIndex, Union, inj, type (:>))
import Data.Functor.Coyoneda (Coyoneda (Coyoneda), hoistCoyoneda, liftCoyoneda, lowerCoyoneda)
import Data.Kind (Type)

newtype Eff ff es a = Eff {unEff :: ff (Union es (Eff ff es)) a}

deriving instance (Functor (ff (Union es (Eff ff es)))) => Functor (Eff ff es)
deriving instance (Applicative (ff (Union es (Eff ff es)))) => Applicative (Eff ff es)
deriving instance (Monad (ff (Union es (Eff ff es)))) => Monad (Eff ff es)

instance (Emb IO `In` es, Monad (Eff ff es), Free c ff) => MonadIO (Eff ff es) where
    liftIO = emb
    {-# INLINE liftIO #-}

convertEff
    :: forall ff gg es a c c'
     . (Free c ff, Free c' gg, c (gg (Union es (Eff gg es))))
    => Eff ff es a
    -> Eff gg es a
convertEff = loop
  where
    loop :: Eff ff es ~> Eff gg es
    loop (Eff a) = Eff $ runFree (liftFree . hfmap loop) a

class (forall f. c (ff f)) => Free c (ff :: (Type -> Type) -> Type -> Type) | ff -> c where
    {-# MINIMAL liftFree, (runFree | (retract, hoist)) #-}

    liftFree :: f a -> ff f a
    runFree :: (c g) => (forall x. f x -> g x) -> ff f a -> g a
    retract :: (c f) => ff f a -> f a
    hoist :: (forall x. f x -> g x) -> ff f a -> ff g a

    runFree f = retract . hoist f
    retract = runFree id

    default hoist :: (c (ff g)) => (forall x. f x -> g x) -> ff f a -> ff g a
    hoist phi = runFree $ liftFree . phi

    {-# INLINE runFree #-}
    {-# INLINE retract #-}
    {-# INLINE hoist #-}

perform :: forall e es ff a c. (e :> es, Free c ff) => e (Eff ff es) a -> Eff ff es a
perform = Eff . liftFree . inj @(LabelIndex e es)
{-# INLINE perform #-}

perform' :: forall key e es ff a c. (Has key e es, Free c ff) => e (Eff ff es) a -> Eff ff es a
perform' = Eff . liftFree . inj @(KeyIndex key es)
{-# INLINE perform' #-}

send :: forall e es ff a c. (e `In` es, Free c ff) => e (Eff ff es) a -> Eff ff es a
send = Eff . liftFree . inj @(Index e es)
{-# INLINE send #-}

emb :: forall f es ff a c. (Emb f `In` es, Free c ff) => f a -> Eff ff es a
emb = send . Emb
{-# INLINE emb #-}

instance Free Functor Coyoneda where
    liftFree = liftCoyoneda
    runFree f (Coyoneda g x) = g <$> f x
    retract = lowerCoyoneda
    hoist phi = hoistCoyoneda phi

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE retract #-}
    {-# INLINE hoist #-}

instance Free Applicative Tree.Ap where
    liftFree = Tree.liftAp
    runFree f = Tree.runAp f
    retract = Tree.retractAp
    hoist phi = Tree.hoistAp phi

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE retract #-}
    {-# INLINE hoist #-}

instance Free Applicative Fast.Ap where
    liftFree = Fast.liftAp
    runFree f = Fast.runAp f
    retract = Fast.retractAp
    hoist phi = Fast.hoistAp phi

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE retract #-}
    {-# INLINE hoist #-}

instance Free Applicative Final.Ap where
    liftFree = Final.liftAp
    runFree f = Final.runAp f
    retract = Final.retractAp
    hoist phi = Final.hoistAp phi

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE retract #-}
    {-# INLINE hoist #-}

instance Free Alternative Tree.Alt where
    liftFree = Tree.liftAlt
    runFree f = Tree.runAlt f
    hoist phi = Tree.hoistAlt phi

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE hoist #-}

instance Free Alternative Final.Alt where
    liftFree = Final.liftAlt
    runFree f = Final.runAlt f
    hoist phi = Final.hoistAlt phi

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE hoist #-}

-- | A natural transformation.
type f ~> g = forall (x :: Type). f x -> g x

infixr 2 ~>
