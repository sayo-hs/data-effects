-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.RWS where

import Control.Effect.Class.Reader (Reader)
import Control.Effect.Class.State (State)
import Control.Effect.Class.Writer (Writer)

class (Reader r f, Writer w f, State s f) => RWS r w s f
makeEmptyEffect ''RWS
