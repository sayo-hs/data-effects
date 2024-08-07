{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Ergonomic and high-level primitive combinators for effectful concurrent programming.

This operates through the cooperation of v'Feed'/v'Consume' effects, which send and receive data,
and the v'PipeTo' effects, which handles their routing.
This is similar to the shell paradigm in POSIX.
In the pipe operator, each action can be seen as a process that operates autonomously in parallel and
communicates with other processes using channels.
-}
module Data.Effect.Concurrent.Pipe where

import Control.Applicative (Alternative, liftA2)
import Control.Arrow (app)
import Control.Monad (MonadPlus, forever, unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (MonadZip)
import Control.Selective (Selective, select, swapEither)
import Data.Bifunctor (first)
import Data.Coerce (Coercible, coerce)
import Data.Effect.Foldl (
    Folding (Folding),
    FoldingH (FoldingH),
    FoldingMapH (..),
    foldingMapH,
    toFoldingH,
 )
import Data.Foldable (for_)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Functor.Classes (Eq1, Ord1)
import Data.Tuple (swap)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic, Generic1)
import Numeric.Natural (Natural)

data PipeH' a f b where
    PipeTo :: f b -> f c -> PipeH' a f (b, c)
    FstWaitPipeTo :: f b -> f c -> PipeH' a f (b, Maybe c)
    SndWaitPipeTo :: f b -> f c -> PipeH' a f (Maybe b, c)
    RacePipeTo :: f b -> f c -> PipeH' a f (Either b c)
    WaitBoth :: f b -> f c -> PipeH' a f (b, c)
    ThenStop :: f b -> f c -> PipeH' a f (b, Maybe c)
    Race :: f b -> f c -> PipeH' a f (Either b c)

data PipeF (a :: Type) (b :: Type) where
    Passthrough :: PipeF a b

data Feed a b where
    Feed :: a -> Feed a ()
    TryFeed :: a -> Feed a Bool

data Consume a b where
    Consume :: Consume a a
    TryConsume :: Consume a (Maybe a)

data Yield a where
    Yield :: Yield ()

data InPlumber a b f c where
    RewriteInflux :: (Either a b -> Either a b) -> f c -> InPlumber a b f c
    RewriteInfluxH :: (Either a b -> f (Either a b)) -> f c -> InPlumber a b f c
    JoinInfluxToLeft :: Coercible a b => f c -> InPlumber a b f c
    JoinInfluxToRight :: Coercible a b => f c -> InPlumber a b f c
    SwapInflux :: Coercible a b => f c -> InPlumber a b f c

data OutPlumber a b f c where
    RewriteOutflux :: (Either a b -> Either a b) -> f c -> OutPlumber a b f c
    RewriteOutfluxH :: (Either a b -> f (Either a b)) -> f c -> OutPlumber a b f c
    JoinOutfluxToLeft :: Coercible a b => f c -> OutPlumber a b f c
    JoinOutfluxToRight :: Coercible a b => f c -> OutPlumber a b f c
    SwapOutflux :: Coercible a b => f c -> OutPlumber a b f c

makeKeyedEffect [] [''PipeH']
makeEffectF [''PipeF, ''Feed, ''Consume, ''Yield]
makeEffectH [''InPlumber, ''OutPlumber]

type PipeComm a f =
    ( SendSigBy PipeHKey (PipeH' a) f
    , PipeF a <: f
    , Feed a <: f
    , Consume a <: f
    , Yield <: f
    )

newtype Connection a = Connection {unConnection :: Maybe a}
    deriving newtype
        ( Eq
        , Ord
        , Functor
        , Foldable
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadZip
        , MonadFail
        , MonadFix
        , Semigroup
        , Monoid
        , Eq1
        , Ord1
        )
    deriving stock (Traversable, Show, Read, Generic, Generic1)

pattern OpenPipe :: a -> Connection a
pattern OpenPipe a = Connection (Just a)

pattern ClosePipe :: Connection a
pattern ClosePipe = Connection Nothing

{-# COMPLETE OpenPipe, ClosePipe #-}

infixl 1 |>
(|>) :: SendSigBy PipeHKey (PipeH' a) f => f b -> f c -> f (b, c)
(|>) = pipeTo
{-# INLINE (|>) #-}

infixl 1 *|>
(*|>) :: SendSigBy PipeHKey (PipeH' a) f => f b -> f c -> f (b, Maybe c)
(*|>) = fstWaitPipeTo
{-# INLINE (*|>) #-}

infixl 1 |*>
(|*>) :: SendSigBy PipeHKey (PipeH' a) f => f b -> f c -> f (Maybe b, c)
(|*>) = sndWaitPipeTo
{-# INLINE (|*>) #-}

infixl 1 *|*>
(*|*>) :: SendSigBy PipeHKey (PipeH' a) f => f b -> f c -> f (Either b c)
(*|*>) = racePipeTo
{-# INLINE (*|*>) #-}

infixr 0 <|
(<|) :: (SendSigBy PipeHKey (PipeH' a) f, Functor f) => f b -> f c -> f (b, c)
a <| b = swap <$> pipeTo b a
{-# INLINE (<|) #-}

infixr 0 <|*
(<|*) :: (SendSigBy PipeHKey (PipeH' a) f, Functor f) => f b -> f c -> f (Maybe b, c)
a <|* b = swap <$> fstWaitPipeTo b a
{-# INLINE (<|*) #-}

infixr 0 <*|
(<*|) :: (SendSigBy PipeHKey (PipeH' a) f, Functor f) => f b -> f c -> f (b, Maybe c)
a <*| b = swap <$> sndWaitPipeTo b a
{-# INLINE (<*|) #-}

infixr 0 <*|*
(<*|*) :: (SendSigBy PipeHKey (PipeH' a) f, Functor f) => f b -> f c -> f (Either b c)
a <*|* b = swapEither <$> racePipeTo b a
{-# INLINE (<*|*) #-}

infixl 1 |||
(|||) :: SendSigBy PipeHKey (PipeH' a) f => f b -> f c -> f (Either b c)
(|||) = race
{-# INLINE (|||) #-}

infixl 1 *||
(*||) :: SendSigBy PipeHKey (PipeH' a) f => f b -> f c -> f (b, Maybe c)
(*||) = thenStop
{-# INLINE (*||) #-}

infixr 0 ||*
(||*) :: (SendSigBy PipeHKey (PipeH' a) f, Functor f) => f b -> f c -> f (Maybe b, c)
a ||* b = swap <$> thenStop b a
{-# INLINE (||*) #-}

infixl 1 *|*
(*|*) :: SendSigBy PipeHKey (PipeH' a) f => f b -> f c -> f (b, c)
(*|*) = waitBoth
{-# INLINE (*|*) #-}

defaultFeed :: (Feed a <: m, Yield <: m, Monad m) => a -> m ()
defaultFeed a = do
    success <- tryFeed a
    unless success do
        yield
        defaultFeed a

defaultConsume :: (Consume a <: m, Yield <: m, Monad m) => m a
defaultConsume = do
    tryConsume >>= \case
        Just a -> pure a
        Nothing -> do
            yield
            defaultConsume

defaultPassthrough :: forall a m. (Feed a <: m, Consume a <: m, Yield <: m, Monad m) => m a
defaultPassthrough =
    forever do
        consume @a >>= feed
        yield

newtype Concurrently f a = Concurrently {runConcurrently :: f a}
    deriving (Functor)

instance (SendSigBy PipeHKey (PipeH' a) f, Applicative f) => Applicative (Concurrently f) where
    pure = Concurrently . pure
    liftA2 f (Concurrently a) (Concurrently b) =
        Concurrently $ uncurry f <$> (a *|* b)
    {-# INLINE pure #-}
    {-# INLINE liftA2 #-}

instance (SendSigBy PipeHKey (PipeH' a) f, Selective f) => Selective (Concurrently f) where
    select (Concurrently x) (Concurrently y) =
        Concurrently $
            select
                (x *|* y <&> \(x', y') -> first (y',) x')
                (pure app)
    {-# INLINE select #-}

timesConcurrently ::
    forall a f m.
    (SendSigBy PipeHKey (PipeH' a) f, Applicative f, Monoid m) =>
    Natural ->
    f m ->
    f m
timesConcurrently n a = case n of
    0 -> pure mempty
    1 -> a
    _ -> runConcurrently $ liftA2 (<>) (Concurrently a) (Concurrently $ timesConcurrently (n - 1) a)

mergeInfluxToLeft :: InPlumber a b <<: f => (b -> a) -> f c -> f c
mergeInfluxToLeft f = rewriteInflux $ either Left (Left . f)
{-# INLINE mergeInfluxToLeft #-}

mergeInfluxToRight :: InPlumber a b <<: f => (a -> b) -> f c -> f c
mergeInfluxToRight f = rewriteInflux $ either (Right . f) Right
{-# INLINE mergeInfluxToRight #-}

exchangeInflux :: InPlumber a b <<: f => (a -> b) -> (b -> a) -> f c -> f c
exchangeInflux f g = rewriteInflux $ either (Right . f) (Left . g)
{-# INLINE exchangeInflux #-}

mergeOutfluxToLeft :: OutPlumber a b <<: f => (b -> a) -> f c -> f c
mergeOutfluxToLeft f = rewriteOutflux $ either Left (Left . f)
{-# INLINE mergeOutfluxToLeft #-}

mergeOutfluxToRight :: OutPlumber a b <<: f => (a -> b) -> f c -> f c
mergeOutfluxToRight f = rewriteOutflux $ either (Right . f) Right
{-# INLINE mergeOutfluxToRight #-}

exchangeOutflux :: OutPlumber a b <<: f => (a -> b) -> (b -> a) -> f c -> f c
exchangeOutflux f g = rewriteOutflux $ either (Right . f) (Left . g)
{-# INLINE exchangeOutflux #-}

defaultJoinInfluxToLeft :: forall a b f c. (InPlumber a b <<: f, Coercible a b) => f c -> f c
defaultJoinInfluxToLeft = mergeInfluxToLeft @a @b coerce
{-# INLINE defaultJoinInfluxToLeft #-}

defaultJoinInfluxToRight :: forall a b f c. (InPlumber a b <<: f, Coercible a b) => f c -> f c
defaultJoinInfluxToRight = mergeInfluxToLeft @a @b coerce
{-# INLINE defaultJoinInfluxToRight #-}

defaultSwapInflux :: forall a b f c. (InPlumber a b <<: f, Coercible a b) => f c -> f c
defaultSwapInflux = exchangeInflux @a @b coerce coerce
{-# INLINE defaultSwapInflux #-}

defaultJoinOutfluxToLeft :: forall a b f c. (OutPlumber a b <<: f, Coercible a b) => f c -> f c
defaultJoinOutfluxToLeft = mergeOutfluxToLeft @a @b coerce
{-# INLINE defaultJoinOutfluxToLeft #-}

defaultJoinOutfluxToRight :: forall a b f c. (OutPlumber a b <<: f, Coercible a b) => f c -> f c
defaultJoinOutfluxToRight = mergeOutfluxToLeft @a @b coerce
{-# INLINE defaultJoinOutfluxToRight #-}

defaultSwapOutflux :: forall a b f c. (OutPlumber a b <<: f, Coercible a b) => f c -> f c
defaultSwapOutflux = exchangeOutflux @a @b coerce coerce
{-# INLINE defaultSwapOutflux #-}

defaultFolding :: (Consume (Connection a) <: m, Monad m) => Folding a b -> m b
defaultFolding (Folding step initial) =
    flip fix initial \next acc -> do
        consume >>= \case
            OpenPipe a -> next $ step acc a
            ClosePipe -> pure acc

defaultFoldingH :: (Consume (Connection a) <: m, Monad m) => FoldingH a m b -> m b
defaultFoldingH (FoldingH step initial) =
    flip fix initial \next acc -> do
        consume >>= \case
            OpenPipe a -> next =<< step acc a
            ClosePipe -> pure acc

defaultFoldingMapH :: (Consume (Connection a) <: m, Monad m) => FoldingMapH a m b -> m b
defaultFoldingMapH = defaultFoldingH . toFoldingH
{-# INLINE defaultFoldingMapH #-}

data Streaming a b c where
    Streaming :: (a -> b) -> Streaming a b ()
makeEffectF [''Streaming]

defaultStreaming :: Streaming a b c -> Filtering a b c
defaultStreaming (Streaming f) = Filtering $ Just . f
{-# INLINE defaultStreaming #-}

data Filtering a b c where
    Filtering :: (a -> Maybe b) -> Filtering a b ()
makeEffectF [''Filtering]

defaultFiltering :: Filtering a b c -> Processing a b c
defaultFiltering (Filtering f) = Processing $ maybe V.empty V.singleton . f
{-# INLINE defaultFiltering #-}

data Processing a b c where
    Processing :: (a -> Vector b) -> Processing a b ()
makeEffectF [''Processing]

defaultProcessing ::
    (Foldable t, FoldingMapH a <<: f, Feed b <: f, Applicative f) =>
    (a -> t b) ->
    f ()
defaultProcessing f =
    foldingMapH \a -> for_ (f a) feed
{-# INLINE defaultProcessing #-}

foldConcurrent ::
    (FoldingMapH a <<: f, SendSigBy PipeHKey (PipeH' a) f, Applicative f) =>
    Natural ->
    FoldingMapH a f m ->
    f m
foldConcurrent nWorkers (FoldingMapH f) =
    timesConcurrently nWorkers (foldingMapH f)
{-# INLINE foldConcurrent #-}
