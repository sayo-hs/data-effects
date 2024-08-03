{-# LANGUAGE AllowAmbiguousTypes #-}

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
import Control.Selective (Selective, select)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.These (These)
import Data.These.Combinators (swapThese)
import Data.Tuple (swap)
import Numeric.Natural (Natural)

data Pipe f a where
    PipeTo :: f a -> f b -> Pipe f (These a b)
    FstBlockingPipeTo :: f a -> f b -> Pipe f (a, Maybe b)
    SndBlockingPipeTo :: f a -> f b -> Pipe f (Maybe a, b)
    BlockingPipeTo :: f a -> f b -> Pipe f (a, b)
    Race :: f a -> f b -> Pipe f (These a b)
    ThenStop :: f a -> f b -> Pipe f (a, Maybe b)
    WaitBoth :: f a -> f b -> Pipe f (a, b)
makeEffectH [''Pipe]

infixl 1 |>
(|>) :: Pipe <<: f => f a -> f b -> f (These a b)
(|>) = pipeTo
{-# INLINE (|>) #-}

infixl 1 *|>
(*|>) :: Pipe <<: f => f a -> f b -> f (a, Maybe b)
(*|>) = fstBlockingPipeTo
{-# INLINE (*|>) #-}

infixl 1 |*>
(|*>) :: Pipe <<: f => f a -> f b -> f (Maybe a, b)
(|*>) = sndBlockingPipeTo
{-# INLINE (|*>) #-}

infixl 1 *|*>
(*|*>) :: Pipe <<: f => f a -> f b -> f (a, b)
(*|*>) = blockingPipeTo
{-# INLINE (*|*>) #-}

infixr 0 <|
(<|) :: (Pipe <<: f, Functor f) => f a -> f b -> f (These a b)
a <| b = swapThese <$> pipeTo b a
{-# INLINE (<|) #-}

infixr 0 <|*
(<|*) :: (Pipe <<: f, Functor f) => f a -> f b -> f (Maybe a, b)
a <|* b = swap <$> fstBlockingPipeTo b a
{-# INLINE (<|*) #-}

infixr 0 <*|
(<*|) :: (Pipe <<: f, Functor f) => f a -> f b -> f (a, Maybe b)
a <*| b = swap <$> sndBlockingPipeTo b a
{-# INLINE (<*|) #-}

infixr 0 <*|*
(<*|*) :: (Pipe <<: f, Functor f) => f a -> f b -> f (a, b)
a <*|* b = swap <$> blockingPipeTo b a
{-# INLINE (<*|*) #-}

infixl 1 |||
(|||) :: Pipe <<: f => f a -> f b -> f (These a b)
(|||) = race
{-# INLINE (|||) #-}

infixl 1 *||
(*||) :: Pipe <<: f => f a -> f b -> f (a, Maybe b)
(*||) = thenStop
{-# INLINE (*||) #-}

infixr 0 ||*
(||*) :: (Pipe <<: f, Functor f) => f a -> f b -> f (Maybe a, b)
a ||* b = swap <$> thenStop b a
{-# INLINE (||*) #-}

infixl 1 *|*
(*|*) :: Pipe <<: f => f a -> f b -> f (a, b)
(*|*) = waitBoth
{-# INLINE (*|*) #-}

newtype Concurrently f a = Concurrently {runConcurrently :: f a}
    deriving (Functor)

instance (Pipe <<: f, Applicative f) => Applicative (Concurrently f) where
    pure = Concurrently . pure
    liftA2 f (Concurrently a) (Concurrently b) =
        Concurrently $ uncurry f <$> (a *|* b)
    {-# INLINE pure #-}
    {-# INLINE liftA2 #-}

instance (Pipe <<: f, Selective f) => Selective (Concurrently f) where
    select (Concurrently x) (Concurrently y) =
        Concurrently $
            select
                (x *|* y <&> \(x', y') -> first (y',) x')
                (pure app)
    {-# INLINE select #-}

timesConcurrently :: forall f m. (Pipe <<: f, Applicative f, Monoid m) => Natural -> f m -> f m
timesConcurrently n a = case n of
    0 -> pure mempty
    1 -> a
    _ -> runConcurrently $ liftA2 (<>) (Concurrently a) (Concurrently $ timesConcurrently (n - 1) a)

data Chan' src dst i o a where
    Send :: forall src dst i o. dst -> o -> Chan' src dst i o ()
    Recv :: forall src dst i o. src -> Chan' src dst i o i
    TryRecv :: forall src dst i o. src -> Chan' src dst i o (Maybe i)
makeKeyedEffect [''Chan'] []

type BroadChan src dst i o = Chan (Maybe src) (Maybe dst) (src, i) o
type BroadChan' src dst i o = Chan' (Maybe src) (Maybe dst) (src, i) o

type EndoChan chan a = Chan chan chan a a
type BroadEndoChan chan a = BroadChan chan chan a a

broadcast :: SendInsBy ChanKey (BroadChan' src dst i o) f => o -> f ()
broadcast = send Nothing
{-# INLINE broadcast #-}

recvAny :: SendInsBy ChanKey (BroadChan' src dst i o) f => f (src, i)
recvAny = recv Nothing
{-# INLINE recvAny #-}

tryRecvAny :: SendInsBy ChanKey (BroadChan' src dst i o) f => f (Maybe (src, i))
tryRecvAny = tryRecv Nothing
{-# INLINE tryRecvAny #-}

broadcastEffect :: o -> BroadChan' src dst i o ()
broadcastEffect = Send Nothing
{-# INLINE broadcastEffect #-}

recvAnyEffect :: BroadChan' src dst i o (src, i)
recvAnyEffect = Recv Nothing
{-# INLINE recvAnyEffect #-}

tryRecvAnyEffect :: BroadChan' src dst i o (Maybe (src, i))
tryRecvAnyEffect = TryRecv Nothing
{-# INLINE tryRecvAnyEffect #-}

data Router' src dst i o f (a :: Type) where
    Preroute :: forall src dst i o f a. (src -> Maybe src) -> f a -> Router' src dst i o f a
    Postroute :: forall src dst i o f a. (dst -> Maybe dst) -> f a -> Router' src dst i o f a
    Loopback :: forall src dst io f a. (dst -> Maybe src) -> f a -> Router' src dst io io f a
makeKeyedEffect [] [''Router']

type EndoRouter' chan a = Router' chan chan a a
type EndoRouter chan a = Router chan chan a a

preforward :: SendSigBy RouterKey (Router' src dst i o) f => (src -> src) -> f a -> f a
preforward f = preroute $ Just . f
{-# INLINE preforward #-}

postforward :: SendSigBy RouterKey (Router' src dst i o) f => (dst -> dst) -> f a -> f a
postforward f = postroute $ Just . f
{-# INLINE postforward #-}

recurrent :: SendSigBy RouterKey (Router' src dst io io) f => (dst -> src) -> f a -> f a
recurrent f = loopback $ Just . f
{-# INLINE recurrent #-}

disconnectIn :: SendSigBy RouterKey (Router' src dst i o) f => f a -> f a
disconnectIn = preroute $ const Nothing
{-# INLINE disconnectIn #-}

disconnectOut :: SendSigBy RouterKey (Router' src dst i o) f => f a -> f a
disconnectOut = postroute $ const Nothing
{-# INLINE disconnectOut #-}

isolate :: SendSigBy RouterKey (Router' src dst i o) f => f a -> f a
isolate = disconnectOut . disconnectIn
{-# INLINE isolate #-}

preforwardEffect :: (src -> src) -> f a -> Router' src dst i o f a
preforwardEffect f = Preroute (Just . f)
{-# INLINE preforwardEffect #-}

postforwardEffect :: (dst -> dst) -> f a -> Router' src dst i o f a
postforwardEffect f = Postroute (Just . f)
{-# INLINE postforwardEffect #-}

recurrentEffect :: (dst -> src) -> f a -> Router' src dst io io f a
recurrentEffect f = Loopback (Just . f)
{-# INLINE recurrentEffect #-}

disconnectInEffect :: f a -> Router' src dst i o f a
disconnectInEffect = Preroute $ const Nothing
{-# INLINE disconnectInEffect #-}

disconnectOutEffect :: f a -> Router' src dst i o f a
disconnectOutEffect = Postroute $ const Nothing
{-# INLINE disconnectOutEffect #-}

data Filter' src dst i o f (a :: Type) where
    Prefilter :: forall src dst i o f a. (src -> i -> f (Maybe (src, i))) -> f a -> Filter' src dst i o f a
    Postfilter :: forall src dst i o f a. (dst -> o -> f (Maybe (dst, o))) -> f a -> Filter' src dst i o f a
    Loopfilter :: forall src dst i o f a. (dst -> o -> f (Maybe (src, i))) -> f a -> Filter' src dst i o f a
makeKeyedEffect [] [''Filter']

type EndoFilter' chan a = Filter' chan chan a a
type EndoFilter chan a = Filter chan chan a a

data PipeProxy src dst i o (a :: Type) where
    PipeProxy :: forall src dst i o a. (src -> i -> Maybe (dst, o)) -> PipeProxy src dst i o a
makeEffectF [''PipeProxy]

data PipeProxyH src dst i o f (a :: Type) where
    PipeProxyH :: forall src dst i o f a. (src -> i -> f (Maybe (dst, o))) -> PipeProxyH src dst i o f a
makeEffectF [''PipeProxyH]

data Processing' src dst i o f (a :: Type) where
    PreProcess :: forall src dst i o f a. (src -> i -> f (src, i)) -> f a -> Processing' src dst i o f a
    PostProcess :: forall src dst i o f a. (dst -> o -> f (dst, o)) -> f a -> Processing' src dst i o f a
    LoopProcess :: forall src dst i o f a. (dst -> o -> f (src, i)) -> f a -> Processing' src dst i o f a
makeKeyedEffect [] [''Processing']

type EndoProcessing' chan a = Processing' chan chan a a
type EndoProcessing chan a = Processing chan chan a a

defaultProcessing ::
    forall src dst i o f a.
    Functor f =>
    Processing' src dst i o f a ->
    Filter' src dst i o f a
defaultProcessing = \case
    PreProcess f a -> Prefilter ((fmap Just .) . f) a
    PostProcess f a -> Postfilter ((fmap Just .) . f) a
    LoopProcess f a -> Loopfilter ((fmap Just .) . f) a
{-# INLINE defaultProcessing #-}

data Streaming src dst i o (a :: Type) where
    Streaming :: forall src dst i o a. (src -> i -> (dst, o)) -> Streaming src dst i o a
makeEffectF [''Streaming]

defaultStreaming :: Streaming src dst i o a -> PipeProxy src dst i o a
defaultStreaming (Streaming f) = PipeProxy ((Just .) . f)
{-# INLINE defaultStreaming #-}

data StreamingH src dst i o f (a :: Type) where
    StreamingH :: forall src dst i o f a. (src -> i -> f (dst, o)) -> StreamingH src dst i o f a
makeEffectH [''StreamingH]

defaultStreamingH ::
    forall src dst i o f a.
    Functor f =>
    StreamingH src dst i o f a ->
    PipeProxyH src dst i o f a
defaultStreamingH (StreamingH f) = PipeProxyH ((fmap Just .) . f)
{-# INLINE defaultStreamingH #-}
