{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect where

import Control.Alternative.Free qualified as Tree
import Control.Alternative.Free.Final qualified as Final
import Control.Applicative (Alternative, empty, (<|>))
import Control.Applicative.Free qualified as Tree
import Control.Applicative.Free.Fast qualified as Fast
import Control.Applicative.Free.Final qualified as Final
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader (MonadReader (ask), local)
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Writer (MonadWriter, listen, tell)
import Control.Monad.Writer qualified as Writer
import Data.Effect (
    Ask (Ask),
    Catch (Catch),
    ChooseH (ChooseH),
    Emb (Emb),
    Empty (Empty),
    Fail (Fail),
    Fix (Efix),
    Local (Local),
    State (Get, Put),
    Tell (Tell),
    Throw (Throw),
    UnliftBase (WithRunInBase),
    UnliftIO,
    WriterH (Listen),
 )
import Data.Effect.HFunctor (hfmap)
import Data.Effect.OpenUnion (
    At,
    Has,
    IdentityResolver,
    In,
    KeyDiscriminator,
    KeyResolver,
    KnownIndex,
    KnownOrder,
    LabelResolver,
    Membership,
    Union,
    inject,
    membership,
    membershipAt,
    type (:>),
 )
import Data.Effect.Tag (Tagged (Tag), type (#))
import Data.Functor.Coyoneda (Coyoneda (Coyoneda), hoistCoyoneda, liftCoyoneda, lowerCoyoneda)
import Data.Kind (Type)
import Data.Tuple (swap)
import UnliftIO qualified as IO

newtype Eff ff es a = Eff {unEff :: ff (Union es (Eff ff es)) a}

deriving instance (Functor (ff (Union es (Eff ff es)))) => Functor (Eff ff es)
deriving instance (Applicative (ff (Union es (Eff ff es)))) => Applicative (Eff ff es)
deriving instance (Monad (ff (Union es (Eff ff es)))) => Monad (Eff ff es)

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
perform = sendFor $ membership @LabelResolver
{-# INLINE perform #-}

perform' :: forall key e es ff a c. (Has key e es, Free c ff) => e (Eff ff es) a -> Eff ff es a
perform' = sendFor (membership @KeyResolver @(KeyDiscriminator key)) . Tag
{-# INLINE perform' #-}

perform'' :: forall tag e es ff a c. (e # tag :> es, Free c ff) => e (Eff ff es) a -> Eff ff es a
perform'' = perform . Tag @tag
{-# INLINE perform'' #-}

send :: forall e es ff a c. (e `In` es, Free c ff) => e (Eff ff es) a -> Eff ff es a
send = sendFor $ membership @IdentityResolver
{-# INLINE send #-}

sendAt :: forall i es ff a c. (KnownIndex i es, Free c ff) => At i es (Eff ff es) a -> Eff ff es a
sendAt = sendFor $ membershipAt @i
{-# INLINE sendAt #-}

sendFor
    :: forall e es ff a c
     . (KnownOrder e, Free c ff)
    => Membership e es
    -> e (Eff ff es) a
    -> Eff ff es a
sendFor i = sendUnion . inject i
{-# INLINE sendFor #-}

emb :: forall f es ff a c. (Emb f :> es, Free c ff) => f a -> Eff ff es a
emb = perform . Emb
{-# INLINE emb #-}

sendUnion :: (Free c ff) => Union es (Eff ff es) a -> Eff ff es a
sendUnion = Eff . liftFree
{-# INLINE sendUnion #-}

instance Free Functor Coyoneda where
    liftFree = liftCoyoneda
    runFree f (Coyoneda g x) = g <$> f x
    retract = lowerCoyoneda
    hoist = hoistCoyoneda

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE retract #-}
    {-# INLINE hoist #-}

instance Free Applicative Tree.Ap where
    liftFree = Tree.liftAp
    runFree = Tree.runAp
    retract = Tree.retractAp
    hoist = Tree.hoistAp

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE retract #-}
    {-# INLINE hoist #-}

instance Free Applicative Fast.Ap where
    liftFree = Fast.liftAp
    runFree = Fast.runAp
    retract = Fast.retractAp
    hoist = Fast.hoistAp

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE retract #-}
    {-# INLINE hoist #-}

instance Free Applicative Final.Ap where
    liftFree = Final.liftAp
    runFree = Final.runAp
    retract = Final.retractAp
    hoist = Final.hoistAp

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE retract #-}
    {-# INLINE hoist #-}

instance Free Alternative Tree.Alt where
    liftFree = Tree.liftAlt
    runFree = Tree.runAlt
    hoist = Tree.hoistAlt

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE hoist #-}

instance Free Alternative Final.Alt where
    liftFree = Final.liftAlt
    runFree = Final.runAlt
    hoist = Final.hoistAlt

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}
    {-# INLINE hoist #-}

-- | A natural transformation.
type f ~> g = forall (x :: Type). f x -> g x

infixr 2 ~>

-- | A stateless higher-order handler.
type e ~~> f = e f ~> f

infix 2 ~~>

instance
    (Ask r :> es, Local r :> es, Monad (Eff ff es), Free c ff)
    => MonadReader r (Eff ff es)
    where
    ask = perform Ask
    local f a = perform $ Local f a
    {-# INLINE ask #-}
    {-# INLINE local #-}

instance
    (Tell w :> es, WriterH w :> es, Monoid w, Monad (Eff ff es), Free c ff)
    => MonadWriter w (Eff ff es)
    where
    tell = perform . Tell
    listen a = fmap swap $ perform $ Listen a
    pass = pass . fmap swap
    {-# INLINE tell #-}
    {-# INLINE listen #-}

{- |
For a given scope, uses the function (the first component of the pair returned
by that scope) to modify the accumulated value of that scope, and then
accumulates the result into the current outer scope.

@
pass m = do
    (w, (f, a)) <- listen m
    tell $ f w
    pure a
@
-}
pass
    :: (Tell w :> es, WriterH w :> es, Monad (Eff ff es), Free c ff)
    => Eff ff es (w -> w, a)
    -> Eff ff es a
pass m = do
    (w, (f, a)) <- perform $ Listen m
    perform $ Tell $ f w
    pure a
{-# INLINE pass #-}

instance (State s :> es, Monad (Eff ff es), Free c ff) => MonadState s (Eff ff es) where
    get = perform Get
    put = perform . Put
    {-# INLINE get #-}
    {-# INLINE put #-}

instance
    ( Ask r :> es
    , Local r :> es
    , Tell w :> es
    , WriterH w :> es
    , State s :> es
    , Monoid w
    , Monad (Eff ff es)
    , Free c ff
    )
    => MonadRWS r w s (Eff ff es)

instance
    (Throw e :> es, Catch e :> es, Monad (Eff ff es), Free c ff)
    => MonadError e (Eff ff es)
    where
    throwError = perform . Throw
    catchError a hdl = perform $ Catch a hdl
    {-# INLINE throwError #-}
    {-# INLINE catchError #-}

instance
    (Empty :> es, ChooseH :> es, Applicative (Eff ff es), Free c ff)
    => Alternative (Eff ff es)
    where
    empty = perform Empty
    a <|> b = perform $ ChooseH a b
    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

instance (Empty :> es, ChooseH :> es, Monad (Eff ff es), Free c ff) => MonadPlus (Eff ff es)

instance (Emb IO :> es, Monad (Eff ff es), Free c ff) => MonadIO (Eff ff es) where
    liftIO = emb
    {-# INLINE liftIO #-}

instance (Fail :> es, Monad (Eff ff es), Free c ff) => MonadFail (Eff ff es) where
    fail = perform . Fail
    {-# INLINE fail #-}

instance (Fix :> es, Monad (Eff ff es), Free c ff) => MonadFix (Eff ff es) where
    mfix = perform . Efix
    {-# INLINE mfix #-}

instance
    (UnliftIO :> es, Emb IO :> es, Monad (Eff ff es), Free c ff)
    => IO.MonadUnliftIO (Eff ff es)
    where
    withRunInIO f = perform $ WithRunInBase f
    {-# INLINE withRunInIO #-}
