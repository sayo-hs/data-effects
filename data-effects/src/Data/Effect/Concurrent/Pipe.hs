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

High-level primitive combinators for effectful concurrent programming.

This operates through the cooperation of v'Feed' / v'Consume' effects, which send and receive data,
and the v'PipeTo' effects, which handles their routing.
This is similar to the shell paradigm in POSIX or the t'Control.Arrow.Arrow' type class.
In the pipe operator, each action can be seen as a process that operates autonomously in parallel and
communicates with other processes using channels.

Mathematically, this emulates a traced symmetric monoidal category.
-}
module Data.Effect.Concurrent.Pipe where

import Control.Applicative (Alternative (empty, (<|>)), liftA2)
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

data PipeH f a where
    PipeTo :: f a -> f b -> PipeH f (a, b)
    FstWaitPipeTo :: f a -> f b -> PipeH f (a, Maybe b)
    SndWaitPipeTo :: f a -> f b -> PipeH f (Maybe a, b)
    RacePipeTo :: f a -> f b -> PipeH f (Either a b)
    -- | Execute both actions in parallel and block until both are completed.
    WaitBoth ::
        f a ->
        f b ->
        PipeH f (a, b)
    -- | Execute both actions in parallel and block until the action of the first argument is completed.
    --  The action of the second argument is cancelled when the first argument's action finishes.
    ThenStop ::
        f a ->
        f b ->
        PipeH f (a, Maybe b)
    -- | Execute both actions in parallel and block until either action is completed.
    --  Once one finishes, the other is canceled.
    Race ::
        f a ->
        f b ->
        PipeH f (Either a b)

data PipeF (a :: Type) where
    Passthrough :: PipeF a
    Halt :: PipeF a
    Yield :: PipeF ()

data PipeLineH (p :: Type) f (a :: Type) where
    UnmaskPipe :: forall p f a. f a -> PipeLineH p f a
    MaskPipe :: forall p f a. f a -> PipeLineH p f a
    WithPipeBranchMode :: forall p f a. (PipeBranchMode -> PipeBranchMode) -> f a -> PipeLineH p f a
    PipeLoop :: forall p f a. f a -> PipeLineH p f a

data PipeLineF (p :: Type) a where
    IsPipeMasked :: PipeLineF p Bool
    GetPipeBranchMode :: PipeLineF p PipeBranchMode

{- | In 'WaitBoth', 'ThenStop', and 'Race' effects,
    should the input pipe split (contents are not duplicated) or distribute (contents are duplicated)?
-}
data PipeBranchMode
    = -- | The input pipe is split between each action.
      --  The contents of the pipe are not duplicated,
      --  meaning that content consumed by one action cannot be received by the other actions.
      SplitPipe
    | -- | The input pipe is distributed (contents are duplicated) to each action.
      DistributePipe
    deriving stock (Eq, Ord, Read, Show, Generic)

data Feed p a where
    Feed :: p -> Feed p ()
    TryFeed :: p -> Feed p Bool

data Consume p a where
    Consume :: Consume p p
    TryConsume :: Consume p (Maybe p)

data Plumber p q (a :: Type) where
    RewriteExchange :: (Either p q -> Either p q) -> Plumber p q a
    JoinToLeft :: Coercible p q => Plumber p q a
    JoinToRight :: Coercible p q => Plumber p q a
    SwapPipe :: Coercible p q => Plumber p q a

data PlumberH p q f (a :: Type) where
    RewriteExchangeH :: (Either p q -> f (Either p q)) -> PlumberH p q f a

makeEffectF [''PipeF, ''PipeLineF, ''Feed, ''Consume, ''Plumber]
makeEffectH [''PipeH, ''PipeLineH, ''PlumberH]

type PipeComm p f =
    ( PipeH <<: f
    , PipeF <: f
    , PipeLineF p <: f
    , PipeLineH p <<: f
    , Feed p <: f
    , Consume p <: f
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
(|>) :: PipeH <<: f => f a -> f b -> f (a, b)
(|>) = pipeTo
{-# INLINE (|>) #-}

infixl 1 *|>
(*|>) :: PipeH <<: f => f a -> f b -> f (a, Maybe b)
(*|>) = fstWaitPipeTo
{-# INLINE (*|>) #-}

infixl 1 |*>
(|*>) :: PipeH <<: f => f a -> f b -> f (Maybe a, b)
(|*>) = sndWaitPipeTo
{-# INLINE (|*>) #-}

infixl 1 ||>
(||>) :: PipeH <<: f => f a -> f b -> f (Either a b)
(||>) = racePipeTo
{-# INLINE (||>) #-}

infixr 0 <|
(<|) :: (PipeH <<: f, Functor f) => f a -> f b -> f (a, b)
a <| b = swap <$> pipeTo b a
{-# INLINE (<|) #-}

infixr 0 <|*
(<|*) :: (PipeH <<: f, Functor f) => f a -> f b -> f (Maybe a, b)
a <|* b = swap <$> fstWaitPipeTo b a
{-# INLINE (<|*) #-}

infixr 0 <*|
(<*|) :: (PipeH <<: f, Functor f) => f a -> f b -> f (a, Maybe b)
a <*| b = swap <$> sndWaitPipeTo b a
{-# INLINE (<*|) #-}

infixr 0 <||
(<||) :: (PipeH <<: f, Functor f) => f a -> f b -> f (Either a b)
a <|| b = swapEither <$> racePipeTo b a
{-# INLINE (<||) #-}

infixl 1 *|*
infixl 1 *||
infixr 0 ||*
infixl 1 |||

-- | Execute both actions in parallel and block until both are completed.
(*|*) :: PipeH <<: f => f a -> f b -> f (a, b)
(*|*) = waitBoth
{-# INLINE (*|*) #-}

{- | Execute both actions in parallel and block until the action of the first argument is completed.
    The action of the second argument is cancelled when the first argument's action finishes.
-}
(*||) :: PipeH <<: f => f a -> f b -> f (a, Maybe b)
(*||) = thenStop
{-# INLINE (*||) #-}

{- | Execute both actions in parallel and block until the action of the second argument is completed.
    The action of the first argument is cancelled when the second argument's action finishes.
-}
(||*) :: (PipeH <<: f, Functor f) => f a -> f b -> f (Maybe a, b)
a ||* b = swap <$> thenStop b a
{-# INLINE (||*) #-}

{- | Execute both actions in parallel and block until either action is completed.
    Once one finishes, the other is canceled.
-}
(|||) :: (PipeH <<: f, Functor f) => f a -> f a -> f a
a ||| b = either id id <$> race a b
{-# INLINE (|||) #-}

withSplitMode :: forall p f a. PipeLineH p <<: f => f a -> f a
withSplitMode = withPipeBranchMode @p $ const SplitPipe

withDistributeMode :: forall p f a. PipeLineH p <<: f => f a -> f a
withDistributeMode = withPipeBranchMode @p $ const DistributePipe

defaultFeed :: (Feed p <: m, PipeF <: m, Monad m) => p -> m ()
defaultFeed a = do
    success <- tryFeed a
    unless success do
        yield
        defaultFeed a

defaultConsume :: (Consume p <: m, PipeF <: m, Monad m) => m p
defaultConsume = do
    tryConsume >>= \case
        Just a -> pure a
        Nothing -> do
            yield
            defaultConsume

defaultPassthrough :: forall p m a. (Feed p <: m, Consume p <: m, PipeF <: m, Monad m) => m a
defaultPassthrough =
    forever do
        consume @p >>= feed
        yield

newtype Concurrently f a = Concurrently {runConcurrently :: f a}
    deriving (Functor)

instance (PipeH <<: f, Applicative f) => Applicative (Concurrently f) where
    pure = Concurrently . pure
    liftA2 f (Concurrently a) (Concurrently b) =
        Concurrently $ uncurry f <$> (a *|* b)
    {-# INLINE pure #-}
    {-# INLINE liftA2 #-}

instance (PipeH <<: f, PipeF <: f, Applicative f) => Alternative (Concurrently f) where
    empty = Concurrently halt
    Concurrently a <|> Concurrently b = Concurrently $ a ||| b
    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

instance (PipeH <<: f, Selective f) => Selective (Concurrently f) where
    select (Concurrently x) (Concurrently y) =
        Concurrently $
            select
                (x *|* y <&> \(x', y') -> first (y',) x')
                (pure app)
    {-# INLINE select #-}

timesConcurrently ::
    forall f m.
    (PipeH <<: f, Applicative f, Monoid m) =>
    Natural ->
    f m ->
    f m
timesConcurrently n a = case n of
    0 -> pure mempty
    1 -> a
    _ -> runConcurrently $ liftA2 (<>) (Concurrently a) (Concurrently $ timesConcurrently (n - 1) a)

mergeToLeft :: forall p q f a. Plumber p q <: f => (q -> p) -> f a
mergeToLeft f = rewriteExchange $ either Left (Left . f)
{-# INLINE mergeToLeft #-}

mergeToRight :: forall p q f a. Plumber p q <: f => (p -> q) -> f a
mergeToRight f = rewriteExchange $ either (Right . f) Right
{-# INLINE mergeToRight #-}

exchangePipe :: forall p q f a. Plumber p q <: f => (p -> q) -> (q -> p) -> f a
exchangePipe f g = rewriteExchange $ either (Right . f) (Left . g)
{-# INLINE exchangePipe #-}

defaultJoinToLeft :: forall p q f a. (Plumber p q <: f, Coercible p q) => f a
defaultJoinToLeft = mergeToLeft @p @q coerce
{-# INLINE defaultJoinToLeft #-}

defaultJoinToRight :: forall p q f a. (Plumber p q <: f, Coercible p q) => f a
defaultJoinToRight = mergeToLeft @p @q coerce
{-# INLINE defaultJoinToRight #-}

defaultSwapPipe :: forall p q f a. (Plumber p q <: f, Coercible p q) => f a
defaultSwapPipe = exchangePipe @p @q coerce coerce
{-# INLINE defaultSwapPipe #-}

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
    (FoldingMapH a <<: f, PipeH <<: f, Applicative f) =>
    Natural ->
    FoldingMapH a f m ->
    f m
foldConcurrent nWorkers (FoldingMapH f) =
    timesConcurrently nWorkers (foldingMapH f)
{-# INLINE foldConcurrent #-}
