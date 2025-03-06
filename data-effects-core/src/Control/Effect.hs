{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo contributors
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
import Control.Monad.Cont qualified as Cont
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
    SubJump (Jump, SubFork),
    Tell (Tell),
    Throw (Throw),
    UnliftBase (WithRunInBase),
    UnliftIO,
    WriterH (Listen),
 )
import Data.Effect.HandlerVec (
    HandlerVec,
    Has,
    IdentityResolver,
    In,
    KeyDiscriminator,
    KeyResolver,
    KnownIndex,
    LabelResolver,
    handlerFor,
    hcfmapVec,
    membership,
    vmapVec,
    type (:>),
 )
import Data.Effect.HandlerVec.Rec (At, Membership, membershipAt)
import Data.Effect.Tag (Tagged (Tag), type (#))
import Data.Functor.Coyoneda (Coyoneda (Coyoneda), hoistCoyoneda, liftCoyoneda, lowerCoyoneda)
import Data.Kind (Type)
import Data.Tuple (swap)
import UnliftIO qualified as IO

newtype Eff ff es a = Eff
    { unEff
        :: forall r es'
         . (forall x. Eff ff es x -> Eff ff es' x)
        -> HandlerVec es (Eff ff es) (ff (Eff ff es') r)
        -> ff (Eff ff es') r a
    }

runAllEff
    :: (forall x. Eff ff es x -> Eff ff es' x)
    -> HandlerVec es (Eff ff es) (ff (Eff ff es') r)
    -> Eff ff es a
    -> ff (Eff ff es') r a
runAllEff kk v (Eff m) = m kk v
{-# INLINE runAllEff #-}

perform :: forall e es ff a. (e :> es) => e (Eff ff es) a -> Eff ff es a
perform = sendFor $ membership @LabelResolver
{-# INLINE perform #-}

perform' :: forall key e es ff a. (Has key e es) => e (Eff ff es) a -> Eff ff es a
perform' = sendFor (membership @KeyResolver @(KeyDiscriminator key)) . Tag
{-# INLINE perform' #-}

perform'' :: forall tag e es ff a. (e # tag :> es) => e (Eff ff es) a -> Eff ff es a
perform'' = perform . Tag @tag
{-# INLINE perform'' #-}

send :: forall e es ff a. (e `In` es) => e (Eff ff es) a -> Eff ff es a
send = sendFor $ membership @IdentityResolver
{-# INLINE send #-}

sendAt :: forall i es ff a. (KnownIndex i es) => At i es (Eff ff es) a -> Eff ff es a
sendAt = sendFor $ membershipAt @i
{-# INLINE sendAt #-}

sendFor
    :: forall e es ff a
     . Membership e es
    -> e (Eff ff es) a
    -> Eff ff es a
sendFor i e = Eff \_ v -> handlerFor i v e
{-# INLINE sendFor #-}

emb :: forall f es ff a. (Emb f :> es) => f a -> Eff ff es a
emb = perform . Emb
{-# INLINE emb #-}

-- | A natural transformation.
type f ~> g = forall (x :: Type). f x -> g x

infixr 2 ~>

-- | A stateless handler.
type e ~~> f = e f ~> f

infix 2 ~~>

infixr 3 $
infixr 4 $$

-- | Type-level infix applcation for functors.
type (f :: Type -> Type) $ a = f a

-- | Type-level infix applcation for higher-order functors.
type (h :: (Type -> Type) -> Type -> Type) $$ f = h f

instance
    (Ask r :> es, Local r :> es, Monad (Eff ff es))
    => MonadReader r (Eff ff es)
    where
    ask = perform Ask
    local f a = perform $ Local f a
    {-# INLINE ask #-}
    {-# INLINE local #-}

instance
    (Tell w :> es, WriterH w :> es, Monoid w, Monad (Eff ff es))
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
    :: forall w a es ff
     . (Tell w :> es, WriterH w :> es, Monad (Eff ff es))
    => Eff ff es (w -> w, a)
    -> Eff ff es a
pass m = do
    (w, (f, a)) <- perform $ Listen m
    perform $ Tell $ f w
    pure a
{-# INLINE pass #-}

instance (State s :> es, Monad (Eff ff es)) => MonadState s (Eff ff es) where
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
    )
    => MonadRWS r w s (Eff ff es)

instance
    (Throw e :> es, Catch e :> es, Monad (Eff ff es))
    => MonadError e (Eff ff es)
    where
    throwError = perform . Throw
    catchError a hdl = perform $ Catch a hdl
    {-# INLINE throwError #-}
    {-# INLINE catchError #-}

instance
    (Empty :> es, ChooseH :> es, Applicative (Eff ff es))
    => Alternative (Eff ff es)
    where
    empty = perform Empty
    a <|> b = perform $ ChooseH a b
    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

instance (Empty :> es, ChooseH :> es, Monad (Eff ff es)) => MonadPlus (Eff ff es)

instance (SubJump ref :> es, Monad (Eff ff es)) => Cont.MonadCont (Eff ff es) where
    callCC = callCC_
    {-# INLINE callCC #-}

sub
    :: forall ref a b es ff
     . (SubJump ref :> es, Monad (Eff ff es))
    => (ref a -> Eff ff es b)
    -> (a -> Eff ff es b)
    -> Eff ff es b
sub p q = perform SubFork >>= either p q
{-# INLINE sub #-}

callCC_
    :: forall ref a b es ff
     . (SubJump ref :> es, Monad (Eff ff es))
    => ((a -> Eff ff es b) -> Eff ff es a)
    -> Eff ff es a
callCC_ f = sub (f . jump) pure
  where
    jump ref x = perform $ Jump ref x
{-# INLINE callCC_ #-}

instance (Emb IO :> es, Monad (Eff ff es)) => MonadIO (Eff ff es) where
    liftIO = emb
    {-# INLINE liftIO #-}

instance (Fail :> es, Monad (Eff ff es)) => MonadFail (Eff ff es) where
    fail = perform . Fail
    {-# INLINE fail #-}

instance (Fix :> es, Monad (Eff ff es)) => MonadFix (Eff ff es) where
    mfix = perform . Efix
    {-# INLINE mfix #-}

instance
    (UnliftIO :> es, Emb IO :> es, Monad (Eff ff es))
    => IO.MonadUnliftIO (Eff ff es)
    where
    withRunInIO f = perform $ WithRunInBase f
    {-# INLINE withRunInIO #-}

-- Free

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

-- | /O(n)/ where /n/ = @length es@
convertEff
    :: forall ff gg es a c c'
     . (Free c ff, Free c' gg, forall r. c (gg r), forall r. c' (ff r))
    => Eff (ViaFree ff) es a
    -> Eff (ViaFree gg) es a
convertEff = loop
  where
    loop :: Eff (ViaFree ff) es ~> Eff (ViaFree gg) es
    loop (Eff f) = Eff \_ v ->
        ViaFree . convertFree . unViaFree $
            f id (hcfmapVec loop . vmapVec (ViaFree . convertFree . unViaFree) $ v)
{-# INLINE convertEff #-}

convertFree :: (Free c ff, Free c' gg, c (gg r)) => ff r a -> gg r a
convertFree = runFree liftFree
{-# INLINE convertFree #-}

newtype ViaFree ff (f :: Type -> Type) (r :: Type -> Type) (a :: Type) = ViaFree {unViaFree :: ff r a}
    deriving newtype (Functor, Applicative, Monad)

instance (forall r. Functor (ff r)) => Functor (Eff (ViaFree ff) es) where
    fmap f (Eff m) = Eff \kk v -> f <$> m kk v
    {-# INLINE fmap #-}

instance (forall r. Applicative (ff r)) => Applicative (Eff (ViaFree ff) es) where
    pure x = Eff \_ _ -> pure x
    Eff ff <*> Eff fm = Eff \kk v -> ff kk v <*> fm kk v
    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance (forall r. Monad (ff r)) => Monad (Eff (ViaFree ff) es) where
    Eff m >>= f = Eff \kk v -> m kk v >>= \x -> unEff (f x) kk v
    {-# INLINE (>>=) #-}

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
