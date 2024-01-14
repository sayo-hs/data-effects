-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides @TemplateHaskell@ functions to derive an instance of the effect that handles
via t'Control.Effect.Class.SendIns'/t'Control.Effect.Class.SendSig' type classes.
-}
module Control.Effect.Class.Machinery.TH.Send (
    module Control.Effect.Class.Machinery.TH.Send,
    EffectOrder (HigherOrder, FirstOrder),
)
where

import Control.Effect.Class.Machinery.TH.Send.Internal (deriveEffectRecv, deriveEffectSend)
import Data.Effect.Class.TH.Internal (EffectOrder (FirstOrder, HigherOrder), reifyEffectInfo)
import Language.Haskell.TH (Dec, Name, Q)

{- |
Derive an instance of the effect that handles via t'Control.Effect.Class.SendIns'/
t'Control.Effect.Class.SendSig' type classes.
-}
makeEffectSend ::
    -- | The class name of the effect.
    Name ->
    -- | The name and order of effect data type corresponding to the effect class.
    Maybe (EffectOrder, Name) ->
    Q [Dec]
makeEffectSend effClsName effDataNameAndOrder = do
    info <- reifyEffectInfo effClsName
    sequence [deriveEffectSend info effDataNameAndOrder]

{- |
Derive instances of t'Control.Effect.Class.SendIns'/t'Control.Effect.Class.SendSig' for
 t`Control.Effect.Class.EffectDataToClass`.
-}
makeEffectRecv ::
    -- | The class name of the effect.
    Name ->
    -- | The order of the effect class.
    EffectOrder ->
    -- | The name of effect data type corresponding to the effect class.
    Name ->
    Q [Dec]
makeEffectRecv effClsName order effDataName = do
    info <- reifyEffectInfo effClsName
    sequence [deriveEffectRecv info order effDataName]
