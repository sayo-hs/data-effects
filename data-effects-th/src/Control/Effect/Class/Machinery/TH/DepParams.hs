-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Class.Machinery.TH.DepParams where

import Control.Effect.Class.Machinery.TH.DepParams.Internal (generateEffectInfoTypeInstances)
import Data.Effect.Class.TH.Internal (EffectOrder, reifyEffectInfo)
import Language.Haskell.TH (Dec, Name, Q)

makeEffectInfoTypeInstances ::
    -- | The class name of the effect.
    Name ->
    -- | The name and order of effect data type corresponding to the effect class.
    Maybe (EffectOrder, Name) ->
    Q [Dec]
makeEffectInfoTypeInstances effClsName effDataNameAndOrder = do
    info <- reifyEffectInfo effClsName
    generateEffectInfoTypeInstances info effDataNameAndOrder
