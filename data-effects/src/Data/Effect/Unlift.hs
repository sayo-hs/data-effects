{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.Unlift where

import Data.Effect.Tag (type (##))

data UnliftBase b f (a :: Type) where
    WithRunInBase :: ((forall x. f x -> b x) -> b a) -> UnliftBase b f a

makeEffectH [''UnliftBase]

type UnliftIO = UnliftBase IO

pattern WithRunInIO :: (f ~> IO -> IO a) -> UnliftIO f a
pattern WithRunInIO f = WithRunInBase f
{-# COMPLETE WithRunInIO #-}

withRunInIO :: UnliftIO <<: f => (f ~> IO -> IO a) -> f a
withRunInIO = withRunInBase
{-# INLINE withRunInIO #-}

withRunInIO' :: forall tag f a. UnliftIO ## tag <<: f => (f ~> IO -> IO a) -> f a
withRunInIO' = withRunInBase' @tag
{-# INLINE withRunInIO' #-}

withRunInIO'' :: forall key f a. SendSigBy key UnliftIO f => (f ~> IO -> IO a) -> f a
withRunInIO'' = withRunInBase'' @key
{-# INLINE withRunInIO'' #-}
