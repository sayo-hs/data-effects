-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
               (c) 2020 Michael Szvetits
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides @TemplateHaskell@ functions to generate the effect data types
(/instruction/s and /signature/s) for effect type classes.
-}
module Data.Effect.Class.TH where

import Control.Monad ((<=<))
import Data.Effect.Class.TH.Internal (
    EffectOrder (FirstOrder, HigherOrder),
    defaultEffectDataNamer,
    generateEffectDataByEffInfo,
    generateLiftInsPatternSynonyms,
    reifyEffectInfo,
 )
import Data.List qualified as L
import Language.Haskell.TH (mkName)
import Language.Haskell.TH.Syntax (Dec, Name, Q, nameBase)

-- | Generate /instruction/ and /signature/ data types from the effect class of the given name.
makeEffectDataWith ::
    -- | An effect order of an effect data type to generate.
    EffectOrder ->
    -- | A name of an effect data type to generate.
    String ->
    -- | The name of the effect class.
    Name ->
    Q [Dec]
makeEffectDataWith order effDataName effClsName =
    fmap (L.singleton . snd)
        . generateEffectDataByEffInfo order (mkName effDataName)
        =<< reifyEffectInfo effClsName

-- | Generate only an /instruction/ data type from the effect class of the given name.
makeInstructionWith :: String -> Name -> Q [Dec]
makeInstructionWith = makeEffectDataWith FirstOrder

-- | Generate only a /signature/ data type from the effect class of the given name.
makeSignatureWith :: String -> Name -> Q [Dec]
makeSignatureWith = makeEffectDataWith HigherOrder

-- | Generate /instruction/ and /signature/ data types from the effect class of the given name.
makeEffectData :: EffectOrder -> Name -> Q [Dec]
makeEffectData order effClsName =
    makeEffectDataWith
        order
        (defaultEffectDataNamer order $ nameBase effClsName)
        effClsName

-- | Generate only an /instruction/ data type from the effect class of the given name.
makeInstruction :: Name -> Q [Dec]
makeInstruction = makeEffectData FirstOrder

-- | Generate only a /signature/ data type from the effect class of the given name.
makeSignature :: Name -> Q [Dec]
makeSignature = makeEffectData HigherOrder

{- |
Generate the pattern synonyms for instruction constructors:
@
    pattern Foobar x y = LiftIns (FoobarI x y)
@ .
-}
makeLiftInsPatternSynonyms :: Name -> Name -> Q [Dec]
makeLiftInsPatternSynonyms dataName = generateLiftInsPatternSynonyms dataName <=< reifyEffectInfo
