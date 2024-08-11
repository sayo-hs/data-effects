{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

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
See also "Data.Effect.Concurrent.Pipe" .
This module adds a new "inversion pipe" to the usual pipe.
In a standard pipe, data flows from upstream to downstream, but the inversion pipe flows in the opposite direction,
contrary to the usual direction defined by the composition operator v'|>'.

Mathematically, while the conventional one emulates a traced symmetric monoidal category,
this emulates a compact closed category.
-}
module Data.Effect.Concurrent.Pipe.Invertible where

import Control.Monad (forever, unless)
import Data.Coerce (Coercible, coerce)
import Data.Data (Data)
import Data.Effect.Concurrent.Pipe (
    Connection,
    PipeF,
    PipeH,
    Yield,
    yield,
    pattern ClosePipe,
    pattern OpenPipe,
 )
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
import GHC.Generics (Generic)

data FeedF p a where
    Feed :: Content p -> FeedF p ()
    TryFeed :: Content p -> FeedF p Bool

data FeedH (p :: Port) f (a :: Type) where
    ConnectOutput :: forall p f a. f a -> FeedH p f a

data ConsumeF p a where
    Consume :: ConsumeF p (Content p)
    TryConsume :: ConsumeF p (Maybe (Content p))

data ConsumeH (p :: Port) f (a :: Type) where
    ConnectInput :: forall p f a. f a -> ConsumeH p f a

data Plumber (p :: Port) (q :: Port) (a :: Type) where
    RewriteExchange ::
        (Either (Content p) (Content q) -> Either (Content p) (Content q)) ->
        Plumber p q a
    JoinToLeft :: Coercible (Content p) (Content q) => Plumber p q a
    JoinToRight :: Coercible (Content p) (Content q) => Plumber p q a
    SwapPipe :: Coercible (Content p) (Content q) => Plumber p q a

data PlumberH (p :: Port) (q :: Port) f (a :: Type) where
    RewriteExchangeH ::
        (Either (Content p) (Content q) -> f (Either (Content p) (Content q))) ->
        PlumberH p q f a

data Port = RegularFlow Type | Inversion Type

type family Content p = r | r -> p where
    Content ( 'RegularFlow a) = (Reg, a)
    Content ( 'Inversion a) = (Inv, a)

data Reg = Reg deriving stock (Eq, Ord, Show, Read, Generic, Data)
newtype Inv = Inv Reg deriving stock (Eq, Ord, Show, Read, Generic, Data)

makeEffectF [''FeedF, ''ConsumeF, ''Plumber]
makeEffectH [''FeedH, ''ConsumeH, ''PlumberH]

type PipeComm a f =
    ( PipeH <<: f
    , PipeF <: f
    , FeedF a <: f
    , FeedH a <<: f
    , ConsumeF a <: f
    , ConsumeH a <<: f
    , Yield <: f
    )

defaultFeed :: (FeedF p <: m, Yield <: m, Monad m) => Content p -> m ()
defaultFeed a = do
    success <- tryFeed a
    unless success do
        yield
        defaultFeed a

defaultConsume :: (ConsumeF p <: m, Yield <: m, Monad m) => m (Content p)
defaultConsume = do
    tryConsume >>= \case
        Just a -> pure a
        Nothing -> do
            yield
            defaultConsume

defaultPassthrough :: forall p m a. (FeedF p <: m, ConsumeF p <: m, Yield <: m, Monad m) => m a
defaultPassthrough =
    forever do
        consume @p >>= feed
        yield

connectPipe :: forall p f a. (FeedH p <<: f, ConsumeH p <<: f) => f a -> f a
connectPipe = connectOutput @p . connectInput @p
{-# INLINE connectPipe #-}

mergeToLeft :: forall p q f a. Plumber p q <: f => (Content q -> Content p) -> f a
mergeToLeft f = rewriteExchange $ either Left (Left . f)
{-# INLINE mergeToLeft #-}

mergeToRight :: forall p q f a. Plumber p q <: f => (Content p -> Content q) -> f a
mergeToRight f = rewriteExchange $ either (Right . f) Right
{-# INLINE mergeToRight #-}

exchangePipe :: forall p q f a. Plumber p q <: f => (Content p -> Content q) -> (Content q -> Content p) -> f a
exchangePipe f g = rewriteExchange $ either (Right . f) (Left . g)
{-# INLINE exchangePipe #-}

defaultJoinToLeft :: forall p q f a. (Plumber p q <: f, Coercible (Content p) (Content q)) => f a
defaultJoinToLeft = mergeToLeft @p @q coerce
{-# INLINE defaultJoinToLeft #-}

defaultJoinToRight :: forall p q f a. (Plumber p q <: f, Coercible (Content p) (Content q)) => f a
defaultJoinToRight = mergeToLeft @p @q coerce
{-# INLINE defaultJoinToRight #-}

defaultSwapPipe :: forall p q f a. (Plumber p q <: f, Coercible (Content p) (Content q)) => f a
defaultSwapPipe = exchangePipe @p @q coerce coerce
{-# INLINE defaultSwapPipe #-}

defaultFolding ::
    forall fl a b m p.
    (ConsumeF p <: m, Content p ~ (fl, Connection a), Monad m) =>
    Folding a b ->
    m b
defaultFolding (Folding step initial) =
    flip fix initial \next acc -> do
        consume @p <&> snd >>= \case
            OpenPipe a -> next $ step acc a
            ClosePipe -> pure acc

defaultFoldingH ::
    forall fl a b m p.
    (ConsumeF p <: m, Content p ~ (fl, Connection a), Monad m) =>
    FoldingH a m b ->
    m b
defaultFoldingH (FoldingH step initial) =
    flip fix initial \next acc -> do
        consume @p <&> snd >>= \case
            OpenPipe a -> next =<< step acc a
            ClosePipe -> pure acc

defaultFoldingMapH ::
    forall fl a b m p.
    (ConsumeF p <: m, Content p ~ (fl, Connection a), Monad m) =>
    FoldingMapH a m b ->
    m b
defaultFoldingMapH = defaultFoldingH @fl . toFoldingH
{-# INLINE defaultFoldingMapH #-}

defaultProcessing ::
    (Foldable t, FoldingMapH a <<: f, FeedF p <: f, Applicative f) =>
    (a -> t (Content p)) ->
    f ()
defaultProcessing f =
    foldingMapH \a -> for_ (f a) feed
{-# INLINE defaultProcessing #-}
