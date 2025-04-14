{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect.Transform where

import Control.Effect (Eff (..), Free, liftFree, sendFor, type (~>))
import Control.Effect.Interpret (interposeFor, interpret, interpretAll, reinterpret)
import Data.Effect.OpenUnion (
    Each,
    Has,
    In,
    KnownLength,
    KnownOrder,
    Membership,
    RemoveHOEs,
    Suffix,
    SuffixUnder,
    Union,
    WeakenHOEs,
    identityMembership,
    keyMembership,
    labelMembership,
    mapUnion,
    prefixFor,
    prefixFor1,
    suffixFor,
    suffixFor1,
    weakenHOEs,
    weakens,
    weakensUnder,
    pattern Here,
    type (++),
    type (:>),
 )
import Data.Effect.Tag (Tagged (Tag), unTag, type (#))

raise :: forall e es a ff c. (Free c ff) => Eff ff es a -> Eff ff (e ': es) a
raise = raises
{-# INLINE raise #-}

raises :: forall es es' a ff c. (Suffix es es', Free c ff) => Eff ff es a -> Eff ff es' a
raises = transAll weakens
{-# INLINE raises #-}

raiseUnder :: forall e0 e1 es a ff c. (Free c ff) => Eff ff (e0 ': es) a -> Eff ff (e0 ': e1 ': es) a
raiseUnder = raisesUnder
{-# INLINE raiseUnder #-}

raisesUnder :: forall e es es' a ff c. (Suffix es es', Free c ff) => Eff ff (e ': es) a -> Eff ff (e ': es') a
raisesUnder = transAll weakensUnder
{-# INLINE raisesUnder #-}

raisesUnders :: forall es es' a ff c. (SuffixUnder es es', Free c ff) => Eff ff es a -> Eff ff es' a
raisesUnders = transAll weakensUnder
{-# INLINE raisesUnders #-}

onlyFirstOrder :: forall es a ff c. (Free c ff, WeakenHOEs es) => Eff ff (RemoveHOEs es) a -> Eff ff es a
onlyFirstOrder = transAll weakenHOEs
{-# INLINE onlyFirstOrder #-}

raisePrefix
    :: forall es' es a ff c
     . (KnownLength es', Free c ff)
    => Eff ff es a
    -> Eff ff (es' ++ es) a
raisePrefix = transAll $ mapUnion $ prefixFor @es'
{-# INLINE raisePrefix #-}

raiseSuffix
    :: forall es' es a ff c
     . (KnownLength es, Free c ff)
    => Eff ff es a
    -> Eff ff (es ++ es') a
raiseSuffix = transAll $ mapUnion $ suffixFor @es'
{-# INLINE raiseSuffix #-}

raisePrefix1
    :: forall fs x es a ff c
     . (KnownLength fs, Free c ff)
    => Eff ff es a
    -> Eff ff (Each fs x ++ es) a
raisePrefix1 = transAll $ mapUnion $ prefixFor1 @fs @x
{-# INLINE raisePrefix1 #-}

raiseSuffix1
    :: forall fs x es a ff c
     . (KnownLength fs, Free c ff)
    => Eff ff (Each fs x) a
    -> Eff ff (Each fs x ++ es) a
raiseSuffix1 = transAll $ mapUnion $ suffixFor1 @fs @x @es
{-# INLINE raiseSuffix1 #-}

transform
    :: forall e e' es a ff c
     . (KnownOrder e, KnownOrder e', Free c ff)
    => (e (Eff ff (e' ': es)) ~> e' (Eff ff (e' ': es)))
    -> Eff ff (e ': es) a
    -> Eff ff (e' ': es) a
transform f = reinterpret $ sendFor Here . f
{-# INLINE transform #-}

translate
    :: forall e e' es a ff c
     . (KnownOrder e, e' :> es, Free c ff)
    => (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translate = translateFor labelMembership
{-# INLINE translate #-}

translateOn
    :: forall key e e' es a ff c
     . (KnownOrder e, Has key e' es, Free c ff)
    => (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translateOn f = translateFor (keyMembership @key) (Tag . f)
{-# INLINE translateOn #-}

translateIn
    :: forall e e' es a ff c
     . (KnownOrder e, e' `In` es, Free c ff)
    => (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translateIn = translateFor identityMembership
{-# INLINE translateIn #-}

translateFor
    :: forall e e' es a ff c
     . (KnownOrder e, KnownOrder e', Free c ff)
    => Membership e' es
    -> (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translateFor i f = interpret $ sendFor i . f
{-# INLINE translateFor #-}

rewrite
    :: forall e es a ff c
     . (e :> es, Free c ff)
    => (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewrite = rewriteFor labelMembership
{-# INLINE rewrite #-}

rewriteOn
    :: forall key e es a ff c
     . (Has key e es, Free c ff)
    => (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewriteOn f = rewriteFor (keyMembership @key) (Tag . f . unTag)
{-# INLINE rewriteOn #-}

rewriteIn
    :: forall e es a ff c
     . (e `In` es, Free c ff)
    => (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewriteIn = rewriteFor identityMembership
{-# INLINE rewriteIn #-}

rewriteFor
    :: forall e es a ff c
     . (KnownOrder e, Free c ff)
    => Membership e es
    -> (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewriteFor i f = interposeFor i (sendFor i . f)
{-# INLINE rewriteFor #-}

transAll
    :: forall es es' ff a c
     . (Free c ff)
    => (Union es (Eff ff es') ~> Union es' (Eff ff es'))
    -> Eff ff es a
    -> Eff ff es' a
transAll f = interpretAll $ Eff . liftFree . f
{-# INLINE transAll #-}

tag
    :: forall tag e es a ff c
     . (KnownOrder e, KnownOrder (e # tag), Free c ff)
    => Eff ff (e ': es) a
    -> Eff ff (e # tag ': es) a
tag = transform Tag
{-# INLINE tag #-}

untag
    :: forall tag e es a ff c
     . (KnownOrder e, KnownOrder (e # tag), Free c ff)
    => Eff ff (e # tag ': es) a
    -> Eff ff (e ': es) a
untag = transform unTag
{-# INLINE untag #-}
