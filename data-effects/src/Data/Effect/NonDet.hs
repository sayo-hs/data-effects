{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   : (c) 2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.NonDet where

data Empty (a :: Type) where
    Empty :: Empty a

makeEffectF [''Empty]

data Choose (a :: Type) where
    Choose :: Choose Bool

makeEffectF [''Choose]

data ChooseH f (a :: Type) where
    ChooseH :: f a -> f a -> ChooseH f a

makeEffectH [''ChooseH]
