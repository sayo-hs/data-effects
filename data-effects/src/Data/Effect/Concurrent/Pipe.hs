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

This operates through the cooperation of `Feed`/"Data.Effect.Foldl" effects, which send and receive data,
and the `Pipe` effect, which handles their routing.
This is similar to the shell paradigm in POSIX.
In the pipe operator, each action can be seen as a process that operates autonomously in parallel and
communicates with other processes using channels.
-}
module Data.Effect.Concurrent.Pipe where

import Control.Applicative (liftA2)
import Control.Arrow (app)
import Control.Selective (Selective, select, swapEither)
import Data.Bifunctor (first)
import Data.Coerce (Coercible, coerce)
import Data.Effect.Foldl (Folding, FoldingMapH (..), foldingMapH)
import Data.Foldable (for_)
import Data.Functor ((<&>))
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

makeKeyedEffect [] [''Pipe']
makeEffectF [''Feed]
makeEffectH [''InPlumber, ''OutPlumber]

type PipeComm a f = (SendSigBy PipeKey (Pipe' a) f, Feed a <: f, Folding a <: f)

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
