-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Key where

import Data.Effect (InsClass, SigClass)

class SendInsBy key f (ins :: InsClass) | key f -> ins where
    sendInsBy :: ins a -> f a

class SendSigBy key f (sig :: SigClass) | key f -> sig where
    sendSigBy :: sig f a -> f a
