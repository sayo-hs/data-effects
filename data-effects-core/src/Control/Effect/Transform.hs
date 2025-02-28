{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect.Transform where

import Control.Effect (Eff (..), Free, type (~>))
import Control.Effect.Interpret (transEff)
import Data.Effect.HandlerVec (
    Each,
    Has,
    In,
    KnownList,
    KnownOrder,
    Membership,
    Suffix,
    SuffixUnder,
    handlerFor,
    identityMembership,
    keyMembership,
    labelMembership,
    override0,
    overrideFor,
    suffix,
    suffixUnder,
    suffixUnders,
    (!:),
    pattern Here,
    type (++),
    type (:>),
 )
import Data.Effect.HandlerVec qualified as V
import Data.Effect.Tag (Tagged (Tag), unTag, type (#))

raise :: forall e es a ff. Eff ff es a -> Eff ff (e ': es) a
raise = raises
{-# INLINE raise #-}

raises :: forall es es' a ff. (Suffix es es') => Eff ff es a -> Eff ff es' a
raises = transEff suffix
{-# INLINE raises #-}

raiseUnder :: forall e0 e1 es a ff. Eff ff (e0 ': es) a -> Eff ff (e0 ': e1 ': es) a
raiseUnder = raisesUnder
{-# INLINE raiseUnder #-}

raisesUnder :: forall e es es' a ff. (Suffix es es') => Eff ff (e ': es) a -> Eff ff (e ': es') a
raisesUnder = transEff suffixUnder
{-# INLINE raisesUnder #-}

raisesUnders :: forall es es' a ff. (SuffixUnder es es') => Eff ff es a -> Eff ff es' a
raisesUnders = transEff suffixUnders
{-# INLINE raisesUnders #-}

raisePrefix
    :: forall es' es a ff
     . (KnownList es')
    => Eff ff es a
    -> Eff ff (es' ++ es) a
raisePrefix = transEff $ V.drop @es'
{-# INLINE raisePrefix #-}

raiseSuffix
    :: forall es' es a ff
     . (KnownList es)
    => Eff ff es a
    -> Eff ff (es ++ es') a
raiseSuffix = transEff $ V.take @es'
{-# INLINE raiseSuffix #-}

raisePrefix1
    :: forall fs x es a ff
     . (KnownList fs)
    => Eff ff es a
    -> Eff ff (Each fs x ++ es) a
raisePrefix1 = transEff $ V.drop1 @fs @x
{-# INLINE raisePrefix1 #-}

raiseSuffix1
    :: forall fs x es a ff
     . (KnownList fs)
    => Eff ff (Each fs x) a
    -> Eff ff (Each fs x ++ es) a
raiseSuffix1 = transEff $ V.take1 @fs @x @es
{-# INLINE raiseSuffix1 #-}

transform
    :: forall e e' es a ff
     . (KnownOrder e)
    => (e (Eff ff (e' ': es)) ~> e' (Eff ff (e' ': es)))
    -> Eff ff (e ': es) a
    -> Eff ff (e' ': es) a
transform f = transEff \v -> override0 (handlerFor Here v . f) v
{-# INLINE transform #-}

translate
    :: forall e e' es a ff
     . (KnownOrder e, e' :> es)
    => (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translate = translateFor labelMembership
{-# INLINE translate #-}

translateOn
    :: forall key e e' es a ff
     . (KnownOrder e, Has key e' es)
    => (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translateOn f = translateFor (keyMembership @key) (Tag . f)
{-# INLINE translateOn #-}

translateIn
    :: forall e e' es a ff
     . (KnownOrder e, e' `In` es)
    => (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translateIn = translateFor identityMembership
{-# INLINE translateIn #-}

translateFor
    :: forall e e' es a ff
     . (KnownOrder e)
    => Membership e' es
    -> (e (Eff ff es) ~> e' (Eff ff es))
    -> Eff ff (e ': es) a
    -> Eff ff es a
translateFor i f = transEff \v -> handlerFor i v . f !: v
{-# INLINE translateFor #-}

rewrite
    :: forall e es a ff c
     . (Free c ff, e :> es)
    => (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewrite = rewriteFor labelMembership
{-# INLINE rewrite #-}

rewriteOn
    :: forall key e es a ff c
     . (Free c ff, Has key e es)
    => (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewriteOn f = rewriteFor (keyMembership @key) (Tag . f . unTag)
{-# INLINE rewriteOn #-}

rewriteIn
    :: forall e es a ff c
     . (Free c ff, e `In` es)
    => (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewriteIn = rewriteFor identityMembership
{-# INLINE rewriteIn #-}

rewriteFor
    :: forall e es a ff
     . (KnownOrder e)
    => Membership e es
    -> (e (Eff ff es) ~> e (Eff ff es))
    -> Eff ff es a
    -> Eff ff es a
rewriteFor i f = transEff \v -> overrideFor i (handlerFor i v . f) v
{-# INLINE rewriteFor #-}

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
