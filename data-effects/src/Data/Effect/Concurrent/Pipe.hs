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

This operates through the cooperation of higher-order effect 'pipes' and first-order effect 'channels'.
This is equivalent to the paradigm of pipes in POSIX.
In the pipe operators, each action can be viewed as an autonomously concurrent process that
communicates with other processes using channels.
-}
module Data.Effect.Concurrent.Pipe where

import Control.Applicative (liftA2)
import Control.Arrow (app)
import Control.Selective (Selective, select, swapEither)
import Data.Bifunctor (first)
import Data.Coerce (Coercible)
import Data.Effect.Foldl (FoldingMapH (..), foldingMapH)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.These (These)
import Data.These.Combinators (swapThese)
import Data.Tuple (swap)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Numeric.Natural (Natural)

data Pipe' a f b where
    PipeTo :: f b -> f c -> Pipe' a f (b, c)
    FstWaitPipeTo :: f b -> f c -> Pipe' a f (b, Maybe c)
    SndWaitPipeTo :: f b -> f c -> Pipe' a f (Maybe b, c)
    RacePipeTo :: f b -> f c -> Pipe' a f (Either b c)
    WaitBoth :: f b -> f c -> Pipe' a f (b, c)
    ThenStop :: f b -> f c -> Pipe' a f (b, Maybe c)
    Race :: f b -> f c -> Pipe' a f (Either b c)

data Feed a b where
    Feed :: a -> Feed a ()

data InPlumber a b f c where
    ZipInflux :: (Either a b -> Either a b) -> f c -> InPlumber a b f c
    ZipInfluxH :: (Either a b -> f (Either a b)) -> f c -> InPlumber a b f c
    MergeInfluxToFst :: Coercible a b => f c -> InPlumber a b f c
    MergeInfluxToSnd :: Coercible a b => f c -> InPlumber a b f c
    SwapInflux :: Coercible a b => f c -> InPlumber a b f c

data OutPlumber a b f c where
    ZipOutflux :: (Either a b -> Either a b) -> f c -> OutPlumber a b f c
    ZipOutfluxH :: (Either a b -> f (Either a b)) -> f c -> OutPlumber a b f c
    MergeOutfluxToFst :: Coercible a b => f c -> OutPlumber a b f c
    MergeOutfluxToSnd :: Coercible a b => f c -> OutPlumber a b f c
    SwapOutflux :: Coercible a b => f c -> OutPlumber a b f c

makeKeyedEffect [] [''Pipe']
makeEffectF [''Feed]
makeEffectH [''InPlumber, ''OutPlumber]

infixl 1 |>
(|>) :: SendSigBy PipeKey (Pipe' a) f => f b -> f c -> f (b, c)
(|>) = pipeTo
{-# INLINE (|>) #-}

infixl 1 *|>
(*|>) :: SendSigBy PipeKey (Pipe' a) f => f b -> f c -> f (b, Maybe c)
(*|>) = fstWaitPipeTo
{-# INLINE (*|>) #-}

infixl 1 |*>
(|*>) :: SendSigBy PipeKey (Pipe' a) f => f b -> f c -> f (Maybe b, c)
(|*>) = sndWaitPipeTo
{-# INLINE (|*>) #-}

infixl 1 *|*>
(*|*>) :: SendSigBy PipeKey (Pipe' a) f => f b -> f c -> f (Either b c)
(*|*>) = racePipeTo
{-# INLINE (*|*>) #-}

infixr 0 <|
(<|) :: (SendSigBy PipeKey (Pipe' a) f, Functor f) => f b -> f c -> f (b, c)
a <| b = swap <$> pipeTo b a
{-# INLINE (<|) #-}

infixr 0 <|*
(<|*) :: (SendSigBy PipeKey (Pipe' a) f, Functor f) => f b -> f c -> f (Maybe b, c)
a <|* b = swap <$> fstWaitPipeTo b a
{-# INLINE (<|*) #-}

infixr 0 <*|
(<*|) :: (SendSigBy PipeKey (Pipe' a) f, Functor f) => f b -> f c -> f (b, Maybe c)
a <*| b = swap <$> sndWaitPipeTo b a
{-# INLINE (<*|) #-}

infixr 0 <*|*
(<*|*) :: (SendSigBy PipeKey (Pipe' a) f, Functor f) => f b -> f c -> f (Either b c)
a <*|* b = swapEither <$> racePipeTo b a
{-# INLINE (<*|*) #-}

infixl 1 |||
(|||) :: SendSigBy PipeKey (Pipe' a) f => f b -> f c -> f (Either b c)
(|||) = race
{-# INLINE (|||) #-}

infixl 1 *||
(*||) :: SendSigBy PipeKey (Pipe' a) f => f b -> f c -> f (b, Maybe c)
(*||) = thenStop
{-# INLINE (*||) #-}

infixr 0 ||*
(||*) :: (SendSigBy PipeKey (Pipe' a) f, Functor f) => f b -> f c -> f (Maybe b, c)
a ||* b = swap <$> thenStop b a
{-# INLINE (||*) #-}

infixl 1 *|*
(*|*) :: SendSigBy PipeKey (Pipe' a) f => f b -> f c -> f (b, c)
(*|*) = waitBoth
{-# INLINE (*|*) #-}

newtype Concurrently f a = Concurrently {runConcurrently :: f a}
    deriving (Functor)

instance (SendSigBy PipeKey (Pipe' a) f, Applicative f) => Applicative (Concurrently f) where
    pure = Concurrently . pure
    liftA2 f (Concurrently a) (Concurrently b) =
        Concurrently $ uncurry f <$> (a *|* b)
    {-# INLINE pure #-}
    {-# INLINE liftA2 #-}

instance (SendSigBy PipeKey (Pipe' a) f, Selective f) => Selective (Concurrently f) where
    select (Concurrently x) (Concurrently y) =
        Concurrently $
            select
                (x *|* y <&> \(x', y') -> first (y',) x')
                (pure app)
    {-# INLINE select #-}

timesConcurrently ::
    forall a f m.
    (SendSigBy PipeKey (Pipe' a) f, Applicative f, Monoid m) =>
    Natural ->
    f m ->
    f m
timesConcurrently n a = case n of
    0 -> pure mempty
    1 -> a
    _ -> runConcurrently $ liftA2 (<>) (Concurrently a) (Concurrently $ timesConcurrently (n - 1) a)

{-
newtype Chan subchan = Chan {unChan :: Maybe subchan}
    deriving newtype (Functor, Foldable, Eq, Ord)
    deriving stock (Traversable, Show)

pattern MainChan :: Chan subchan
pattern MainChan = Chan Nothing

pattern SubChan :: subchan -> Chan subchan
pattern SubChan chan = Chan (Just chan)

{-# COMPLETE MainChan, SubChan #-}

data Routing' src dst f (a :: Type) where
    Preroute :: (Chan src -> [Chan src]) -> f a -> Routing' src dst f a
    Postroute :: (Chan dst -> [Chan dst]) -> f a -> Routing' src dst f a
    Loopback :: (Chan dst -> [Chan src]) -> f a -> Routing' src dst f a
makeKeyedEffect [] [''Routing']

preforward :: SendSigBy RoutingKey (Routing' src dst) f => (src -> src) -> f a -> f a
preforward f = preroute $ Just . f
{-# INLINE preforward #-}

postforward :: SendSigBy RoutingKey (Routing' src dst) f => (dst -> dst) -> f a -> f a
postforward f = postroute $ Just . f
{-# INLINE postforward #-}

recurrent :: SendSigBy RoutingKey (Routing' src dst) f => (dst -> src) -> f a -> f a
recurrent f = loopback $ Just . f
{-# INLINE recurrent #-}

disconnectIn :: SendSigBy RoutingKey (Routing' src dst) f => f a -> f a
disconnectIn = preroute $ const Nothing
{-# INLINE disconnectIn #-}

disconnectOut :: SendSigBy RoutingKey (Routing' src dst) f => f a -> f a
disconnectOut = postroute $ const Nothing
{-# INLINE disconnectOut #-}

isolate :: SendSigBy RoutingKey (Routing' src dst) f => f a -> f a
isolate = disconnectOut . disconnectIn
{-# INLINE isolate #-}

preforwardEffect :: (src -> src) -> f a -> Routing' src dst f a
preforwardEffect f = Preroute (Just . f)
{-# INLINE preforwardEffect #-}

postforwardEffect :: (dst -> dst) -> f a -> Routing' src dst f a
postforwardEffect f = Postroute (Just . f)
{-# INLINE postforwardEffect #-}

recurrentEffect :: (dst -> src) -> f a -> Routing' src dst f a
recurrentEffect f = Loopback (Just . f)
{-# INLINE recurrentEffect #-}

disconnectInEffect :: f a -> Routing' src dst f a
disconnectInEffect = Preroute $ const Nothing
{-# INLINE disconnectInEffect #-}

disconnectOutEffect :: f a -> Routing' src dst f a
disconnectOutEffect = Postroute $ const Nothing
{-# INLINE disconnectOutEffect #-}

data Comm chan a b where
    Send :: chan -> a -> Comm chan a ()
    Recv :: chan -> Comm chan a a
    TryRecv :: chan -> Comm chan a (Maybe a)
makeEffectF [''Comm]

type PipeComm chan a f = (Comm chan a <: f, SendSigBy PipeKey (Pipe' a) f)

data BroadComm chan a b where
    Broadcast :: forall chan a. a -> BroadComm chan a ()
    RecvAny :: BroadComm chan a (chan, a)
    TryRecvAny :: BroadComm chan a (Maybe (chan, a))
makeEffectF [''BroadComm]

type PipeBroadComm chan a f = (PipeComm chan a f, BroadComm chan a <: f)
-}

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
    (FoldingMapH a <<: f, SendSigBy PipeKey (Pipe' a) f, Applicative f) =>
    Natural ->
    FoldingMapH a f m ->
    f m
foldConcurrent nWorkers (FoldingMapH f) =
    timesConcurrently nWorkers (foldingMapH f)
{-# INLINE foldConcurrent #-}
