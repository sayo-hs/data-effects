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
-}
module Data.Effect.Foldl where

import Control.Foldl (EndoM (EndoM), appEndoM)
import Data.Effect.Input (Input, input)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Monoid (Ap (Ap), Endo (Endo), appEndo, getAp)

data Folding a b where
    Folding :: (b -> a -> b) -> b -> Folding a b
makeEffectF [''Folding]

foldingMap :: (Monoid m, Folding a <: f) => (a -> m) -> f m
foldingMap f = folding (\acc x -> acc <> f x) mempty

data FoldingMapH a f b where
    FoldingMapH :: Monoid m => (a -> f m) -> FoldingMapH a f m
makeEffectH [''FoldingMapH]

{-
data FoldingMapH' a f b where
    FoldingMapH' :: (a -> f (b -> b)) -> b -> FoldingMapH' a f b

iso :: Functor f => FoldingMapH a f b -> FoldingMapH' a f b
iso (FoldingMapH f) = FoldingMapH' (fmap (<>) . f) mempty

iso' :: (FoldingMapH a <<: f, Functor f) => FoldingMapH' a f b -> f b
iso' (FoldingMapH' step initial) = foldingMapH (fmap Endo . step) <&> (`appEndo` initial)
-}

data FoldingH a f b where
    FoldingH :: (b -> a -> f b) -> b -> FoldingH a f b
makeEffectH [''FoldingH]

toFoldingMapH :: (FoldingMapH a <<: f, Applicative f) => Folding a b -> f b
toFoldingMapH (Folding step initial) =
    foldingMapH (pure . Endo . flip step) <&> (`appEndo` initial)

toFoldingH :: Functor f => FoldingMapH a f b -> FoldingH a f b
toFoldingH (FoldingMapH f) =
    FoldingH (\acc x -> (acc <>) <$> f x) mempty

fromFoldingMapH :: (Folding a <: m, Monad m) => FoldingMapH a m b -> m b
fromFoldingMapH (FoldingMapH f) =
    folding (flip ((<>) . Ap . f)) mempty >>= getAp

fromFoldingH :: (Folding a <: m, Monad m) => FoldingH a m b -> m b
fromFoldingH (FoldingH step initial) =
    foldingMap (EndoM . flip step) >>= (`appEndoM` initial)

foldInput :: (Input (Maybe a) <: m, Monad m) => Folding a b -> m b
foldInput (Folding step initial) = do
    flip fix initial \next acc -> do
        input >>= \case
            Just a -> next $ step acc a
            Nothing -> pure acc
