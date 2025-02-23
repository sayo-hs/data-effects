{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect.Transform where

import Control.Effect (Eff (..), Free, sendFor, sendUnion, type (~>))
import Control.Effect.Interpret (interposeFor, interpretAll)
import Data.Effect.OpenUnion (
    EachEffect,
    Has,
    In,
    KnownLength,
    KnownOrder,
    Membership,
    Union,
    Weaken,
    WeakenUnder,
    identityMembership,
    inject,
    keyMembership,
    labelMembership,
    mapUnion,
    prefixFor,
    prefixFor1,
    suffixFor,
    suffixFor1,
    weaken,
    weakens,
    weakensUnder,
    (!+),
    (:>),
    pattern Here,
    type (++),
 )
import Data.Effect.Tag (Tagged (Tag), unTag, type (#))

raise :: forall e es a ff c. (Free c ff) => Eff ff es a -> Eff ff (e ': es) a
raise = transAll weaken
{-# INLINE raise #-}

raises :: forall es es' a ff c. (Weaken es es', Free c ff) => Eff ff es a -> Eff ff es' a
raises = transAll weakens
{-# INLINE raises #-}

raisesUnder :: forall es es' a ff c. (WeakenUnder es es', Free c ff) => Eff ff es a -> Eff ff es' a
raisesUnder = transAll weakensUnder
{-# INLINE raisesUnder #-}

raiseUnder :: forall e0 e1 es a ff c. (Free c ff) => Eff ff (e0 ': es) a -> Eff ff (e0 ': e1 ': es) a
raiseUnder = raisesUnder
{-# INLINE raiseUnder #-}

raisePrefix
    :: forall es' es a ff c
     . (Free c ff, KnownLength es')
    => Eff ff es a
    -> Eff ff (es' ++ es) a
raisePrefix = transAll $ mapUnion $ prefixFor @es'
{-# INLINE raisePrefix #-}

raiseSuffix
    :: forall es' es a ff c
     . (Free c ff)
    => Eff ff es a
    -> Eff ff (es ++ es') a
raiseSuffix = transAll $ mapUnion $ suffixFor @es'
{-# INLINE raiseSuffix #-}

raisePrefix1
    :: forall fs x es a ff c
     . (Free c ff, KnownLength fs)
    => Eff ff es a
    -> Eff ff (EachEffect fs x ++ es) a
raisePrefix1 = transAll $ mapUnion $ prefixFor1 @fs @x
{-# INLINE raisePrefix1 #-}

raiseSuffix1
    :: forall fs x es a ff c
     . (Free c ff)
    => Eff ff es a
    -> Eff ff (es ++ EachEffect fs x) a
raiseSuffix1 = transAll $ mapUnion $ suffixFor1 @fs @x
{-# INLINE raiseSuffix1 #-}

transform
    :: forall e e' es a ff c
     . (Free c ff, KnownOrder e, KnownOrder e')
    => (e (Eff ff (e' ': es)) ~> e' (Eff ff (e' ': es)))
    -> Eff ff (e ': es) a
    -> Eff ff (e' ': es) a
transform f = transAll $ inject Here . f !+ weaken
{-# INLINE transform #-}

translate
    :: forall e e' es a ff c
     . (Free c ff, KnownOrder e, KnownOrder e', e' :> es)
    => (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translate = translateFor labelMembership
{-# INLINE translate #-}

translateOn
    :: forall key e e' es a ff c
     . (Free c ff, KnownOrder e, KnownOrder e', Has key e' es)
    => (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translateOn f = translateFor (keyMembership @key) (Tag . f)
{-# INLINE translateOn #-}

translateIn
    :: forall e e' es a ff c
     . (Free c ff, KnownOrder e, KnownOrder e', e' `In` es)
    => (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translateIn = translateFor identityMembership
{-# INLINE translateIn #-}

translateFor
    :: forall e e' es a ff c
     . (Free c ff, KnownOrder e, KnownOrder e')
    => Membership e' es
    -> (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translateFor i f = transAll $ inject i . f !+ id
{-# INLINE translateFor #-}

rewrite
    :: forall e es a ff c
     . (Free c ff, KnownOrder e, e :> es)
    => (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewrite = rewriteFor labelMembership
{-# INLINE rewrite #-}

rewriteOn
    :: forall key e es a ff c
     . (Free c ff, KnownOrder e, Has key e es)
    => (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewriteOn f = rewriteFor (keyMembership @key) (Tag . f . unTag)
{-# INLINE rewriteOn #-}

rewriteIn
    :: forall e es a ff c
     . (Free c ff, KnownOrder e, e `In` es)
    => (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewriteIn = rewriteFor identityMembership
{-# INLINE rewriteIn #-}

rewriteFor
    :: forall e es a ff c
     . (Free c ff, KnownOrder e)
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
transAll f = interpretAll $ sendUnion . f
{-# INLINE transAll #-}

tag
    :: forall tag e es a ff c
     . (Free c ff, KnownOrder e, KnownOrder (e # tag))
    => Eff ff (e ': es) a
    -> Eff ff (e # tag ': es) a
tag = transform Tag
{-# INLINE tag #-}

untag
    :: forall tag e es a ff c
     . (Free c ff, KnownOrder e, KnownOrder (e # tag))
    => Eff ff (e # tag ': es) a
    -> Eff ff (e ': es) a
untag = transform unTag
{-# INLINE untag #-}
