{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- SPDX-License-Identifier: MPL-2.0 AND BSD-3-Clause

{-  The code before modification is licensed under the BSD3 License as
    shown in [1].  The modified code, in its entirety, is licensed under
    MPL 2.0. When redistributing, please ensure that you do not remove
    the BSD3 License text as indicated in [1].
    <https://github.com/re-xyr/speff/blob/705bf6949dcb78a5d486f68c628e42977660278e/src/Sp/Internal/Env.hs>

    [1] Copyright (c) 2021 Xy Ren

        All rights reserved.

        Redistribution and use in source and binary forms, with or without
        modification, are permitted provided that the following conditions are met:

            * Redistributions of source code must retain the above copyright
            notice, this list of conditions and the following disclaimer.

            * Redistributions in binary form must reproduce the above
            copyright notice, this list of conditions and the following
            disclaimer in the documentation and/or other materials provided
            with the distribution.

            * Neither the name of Author name here nor the names of other
            contributors may be used to endorse or promote products derived
            from this software without specific prior written permission.

        THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
        "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
        LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
        A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
        OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
        SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
        LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
        DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
        THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
        (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
        OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{- |
Copyright   :  (c) 2021 Xy Ren
               (c) 2023-2024 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file) AND BSD-3-Clause
Maintainer  :  ymdfield@outlook.jp

This module defines an immutable extensible record type, similar to @vinyl@ and @data-diverse@. However this
implementation focuses on fast reads, hence has very different performance characteristics from other libraries:

* Lookup: Amortized /O/(1).
* Update: /O/(/n/).
* Shrink: /O/(1).
* Append: /O/(/n/).
-}
module Data.Effect.HandlerVec.Rec where

import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.ST (ST)
import Data.Effect (Effect)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.HandlerVec.Vec (Vec (Vec))
import Data.Effect.HandlerVec.Vec qualified as Vec
import Data.Kind (Constraint, Type)
import Data.Primitive.Array (Array, MutableArray, createArray, writeArray)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exts (Any)
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import GHC.TypeNats (KnownNat, natVal, type (-))
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (concat, drop, head, length, tail, take)

-- | Extensible record type supporting efficient /O/(1) reads.
type role Rec representational nominal

newtype Rec (f :: k -> Type) (es :: [k]) = Rec (Vec Any)

-- | Get the length of the record.
length :: Rec f es -> Int
length (Rec vec) = Vec.length vec
{-# INLINE length #-}

unreifiable :: String -> String -> a
unreifiable clsName comp =
    error $
        "Sp.Internal.Env: Attempting to access "
            <> comp
            <> " without a reflected value. This is perhaps because you are \
               \trying to define an instance for the '"
            <> clsName
            <> "' typeclass, which you should not be doing whatsoever. If \
               \that seems unlikely, please report this as a bug."

--------------------------------------------------------------------------------
-- Construction ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Create an empty record. /O/(1).
nil :: Rec f '[]
nil = Rec Vec.empty
{-# INLINE nil #-}

-- | Prepend one entry to the record. /O/(/n/).
cons :: f e -> Rec f es -> Rec f (e ': es)
cons x (Rec vec) = Rec $ Vec.cons (toAny x) vec
{-# INLINE cons #-}

infixr 5 !*
(!*) :: f e -> Rec f es -> Rec f (e ': es)
(!*) = cons
{-# INLINE (!*) #-}

singleton :: f e -> Rec f '[e]
singleton x = Rec $ Vec.singleton (toAny x)
{-# INLINE singleton #-}

generate :: forall f g es. Rec g es -> (forall e. Membership e es -> f e) -> Rec f es
generate v f = Rec $ Vec.generate (length v) (toAny . f . UnsafeMembership)
{-# INLINE generate #-}

{-
generateHFMaps :: forall es g. (HFunctors es) => Rec g es -> Rec HFMap es
generateHFMaps v =
    let len = length v
     in Rec $ Vec 0 len $ createArray len Vec.nil \marr -> writeHFMapVec @es marr 0

newtype HFMap (e :: Effect)
    = HFMap {getHFMap :: forall f g a. (forall x. f x -> g x) -> e f a -> e g a}

class HFunctors (es :: [Effect]) where
    writeHFMapVec :: MutableArray s Any -> Int -> ST s ()

instance (HFunctor e, HFunctors es) => HFunctors (e ': es) where
    writeHFMapVec marr ix = do
        writeArray marr ix $ toAny $ HFMap @e hfmap
        writeHFMapVec @es marr (ix + 1)
    {-# INLINE writeHFMapVec #-}

instance HFunctors '[] where
    writeHFMapVec _ _ = pure ()
    {-# INLINE writeHFMapVec #-}
-}

generateWith
    :: forall c f es g
     . (Forall c es)
    => Rec g es
    -> (forall e. (c e) => Membership e es -> f e)
    -> Rec f es
generateWith v f =
    let len = length v
     in Rec $ Vec 0 len $ createArray len Vec.nil \marr ->
            writeVecWith @c @es marr 0 \(_ :: Proxy e) ix ->
                toAny $ f @e (UnsafeMembership ix)
{-# INLINE generateWith #-}

type Forall :: forall {k}. (k -> Constraint) -> [k] -> Constraint
class Forall c es where
    writeVecWith :: MutableArray s a -> Int -> (forall e. (c e) => Proxy e -> Int -> a) -> ST s ()

instance (c e, Forall c es) => Forall c (e ': es) where
    writeVecWith marr ix f = do
        writeArray marr ix (f @e Proxy ix)
        writeVecWith @c @es marr (ix + 1) f
    {-# INLINE writeVecWith #-}

instance Forall c '[] where
    writeVecWith _ _ _ = pure ()
    {-# INLINE writeVecWith #-}

-- | Type level list concatenation.
type family (xs :: [k]) ++ (ys :: [k]) where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

infixr 5 ++

-- | Concatenate two records. /O/(/m/ + /n/).
concat :: Rec f es -> Rec f es' -> Rec f (es ++ es')
concat (Rec vec) (Rec vec') = Rec $ Vec.concat vec vec'
{-# INLINE concat #-}

suffixUnder :: forall e es es' f. (Suffix es es') => Rec f (e ': es') -> Rec f (e ': es)
suffixUnder (Rec v) = Rec $ Vec.removesUnder (prefixLen @es @es') v
{-# INLINE suffixUnder #-}

suffixUnders :: forall es es' f. (SuffixUnder es es') => Rec f es' -> Rec f es
suffixUnders (Rec v) = Rec $ Vec.removesUnders (offset @es @es') (prefixLenUnder @es @es') v
{-# INLINE suffixUnders #-}

removeUnder :: forall e es es' f. (KnownList es) => Rec f (e ': es ++ es') -> Rec f (e ': es')
removeUnder (Rec v) = Rec $ Vec.removesUnder (reifyLen @_ @es) v
{-# INLINE removeUnder #-}

removeUnders :: forall es es' es'' f. (KnownList es, KnownList es') => Rec f (es ++ es' ++ es'') -> Rec f (es ++ es'')
removeUnders (Rec v) = Rec $ Vec.removesUnders (reifyLen @_ @es) (reifyLen @_ @es') v
{-# INLINE removeUnders #-}

--------------------------------------------------------------------------------
-- Deconstruction --------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the head of the record. /O/(1).
head :: Rec f (e ': es) -> f e
head (Rec vec) = fromAny $ Vec.head vec
{-# INLINE head #-}

-- | Slice off one entry from the top of the record. /O/(1).
tail :: Rec f (e ': es) -> Rec f es
tail (Rec vec) = Rec $ Vec.tail vec
{-# INLINE tail #-}

-- | The list @es@ list is concrete, i.e. is of the form @'[a1, a2, ..., an]@ therefore having a known length.
class KnownList (es :: [k]) where
    -- | Get the length of the list.
    reifyLen :: Int

-- reifyLen = unreifiable "KnownList" "the length of a type-level list"

instance KnownList '[] where
    reifyLen = 0

instance (KnownList es) => KnownList (e ': es) where
    reifyLen = 1 + reifyLen @_ @es

-- | Slice off several entries from the top of the record. Amortized /O/(1).
drop :: forall es es' f. (KnownList es) => Rec f (es ++ es') -> Rec f es'
drop (Rec vec) = Rec $ Vec.drop (reifyLen @_ @es) vec
{-# INLINE drop #-}

-- | Take elements from the top of the record. /O/(/m/).
take :: forall es' es f. (KnownList es) => Rec f (es ++ es') -> Rec f es
take (Rec vec) = Rec $ Vec.take (reifyLen @_ @es) vec
{-# INLINE take #-}

type family Each fs x where
    Each (f ': fs) x = (f x ': Each fs x)
    Each '[] x = '[]

drop1 :: forall fs x es f. (KnownList fs) => Rec f (Each fs x ++ es) -> Rec f es
drop1 (Rec v) = Rec $ Vec.drop (reifyLen @_ @fs) v
{-# INLINE drop1 #-}

take1 :: forall fs x es f. (KnownList fs) => Rec f (Each fs x ++ es) -> Rec f (Each fs x)
take1 (Rec v) = Rec $ Vec.take (reifyLen @_ @fs) v
{-# INLINE take1 #-}

--------------------------------------------------------------------------------
-- Retrieval & Update ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get an element in the record. Amortized /O/(1).
index :: forall e es f. Membership e es -> Rec f es -> f e
index (UnsafeMembership i) (Rec vec) = fromAny $ Vec.index i vec
{-# INLINE index #-}

-- | Update an entry in the record. /O/(/n/).
update :: forall e es f. Membership e es -> f e -> Rec f es -> Rec f es
update (UnsafeMembership i) x (Rec vec) = Rec $ Vec.update i (toAny x) vec
{-# INLINE update #-}

update0 :: forall e e' es f. f e' -> Rec f (e ': es) -> Rec f (e' ': es)
update0 x (Rec vec) = Rec $ Vec.update 0 (toAny x) vec
{-# INLINE update0 #-}

modify :: forall e es f. Membership e es -> (f e -> f e) -> Rec f es -> Rec f es
modify (UnsafeMembership i) f (Rec vec) = Rec $ Vec.update i (toAny $ f $ fromAny $ Vec.index i vec) vec
{-# INLINE modify #-}

modify0 :: forall e e' es f. (f e -> f e') -> Rec f (e ': es) -> Rec f (e' ': es)
modify0 f (Rec vec) = Rec $ Vec.update 0 (toAny $ f $ fromAny $ Vec.index 0 vec) vec
{-# INLINE modify0 #-}

map :: forall es f g. (forall x. f x -> g x) -> Rec f es -> Rec g es
map f (Rec v) = Rec $ Vec.map (toAny . f . fromAny) v
{-# INLINE map #-}

-- | Get a suffix of the record. Amortized /O/(1).
suffix :: forall es es' f. (Suffix es es') => Rec f es' -> Rec f es
suffix (Rec vec) = Rec $ Vec.drop (prefixLen @es @es') vec
{-# INLINE suffix #-}

--------------------------------------------------------------------------------
-- Any -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Coerce any boxed value into 'GHC.Exts.Any'.
toAny :: a -> Any
toAny = unsafeCoerce

{- | Coerce an 'GHC.Exts.Any' value to a certain type. This is generally unsafe and it is your responsibility to ensure
that the type you're coercing into is the original type that the 'GHC.Exts.Any' value is coerced from.
-}
fromAny :: Any -> a
fromAny = unsafeCoerce

---

newtype Membership (e :: k) (es :: [k]) = UnsafeMembership {unMembership :: Int}
    deriving stock (Eq, Show)

pattern Here :: Membership e (e ': es)
pattern Here = UnsafeMembership 0
{-# INLINE Here #-}

pattern There :: Membership e es -> Membership e (e ': es)
pattern There i <- ((UnsafeMembership . (`subtract` 1) &&& (/= 0)) . unMembership -> (i, True))
    where
        There (UnsafeMembership n) = UnsafeMembership (n + 1)
{-# INLINE There #-}

{-# COMPLETE Here, There #-}

weakenFor :: Membership e es -> Membership e (e' ': es)
weakenFor (UnsafeMembership n) = UnsafeMembership $ n + 1
{-# INLINE weakenFor #-}

membershipAt :: forall i es. (KnownNat i) => Membership (At i es) es
membershipAt = UnsafeMembership $ intVal @i
{-# INLINE membershipAt #-}

compareMembership :: Membership e es -> Membership e' es -> Maybe (e :~: e')
compareMembership (UnsafeMembership m) (UnsafeMembership n)
    | m == n = Just $ unsafeCoerce Refl
    | otherwise = Nothing
{-# INLINE compareMembership #-}

type family At i es where
    At 0 '[] = TypeError ('Text "Effect index out of range")
    At 0 (e ': es) = e
    At n (e ': es) = At (n - 1) es

intVal :: forall n. (KnownNat n) => Int
intVal = fromIntegral $ natVal @n Proxy
{-# INLINE intVal #-}

weakensFor :: forall es es' e. (Suffix es es') => Membership e es -> Membership e es'
weakensFor (UnsafeMembership n) = UnsafeMembership $ n + prefixLen @es @es'
{-# INLINE weakensFor #-}

type Suffix :: forall {k}. [k] -> [k] -> Constraint
class Suffix (es :: [k]) (es' :: [k]) where
    prefixLen :: Int

instance {-# INCOHERENT #-} Suffix es es where
    prefixLen = 0
    {-# INLINE prefixLen #-}

instance (Suffix es es') => Suffix es (e ': es') where
    prefixLen = prefixLen @es @es' + 1
    {-# INLINE prefixLen #-}

type SuffixUnder :: forall {k}. [k] -> [k] -> Constraint
class SuffixUnder (es :: [k]) (es' :: [k]) where
    prefixLenUnder :: Int
    offset :: Int

instance {-# INCOHERENT #-} (Suffix es es') => SuffixUnder es es' where
    prefixLenUnder = prefixLen @es @es'
    offset = 0

    {-# INLINE prefixLenUnder #-}
    {-# INLINE offset #-}

instance (SuffixUnder es es') => SuffixUnder (e ': es) (e ': es') where
    prefixLenUnder = prefixLenUnder @es @es'
    offset = offset @es @es' + 1

    {-# INLINE prefixLenUnder #-}
    {-# INLINE offset #-}

weakensUnderFor :: forall es es' e. (SuffixUnder es es') => Membership e es -> Membership e es'
weakensUnderFor (UnsafeMembership n) =
    UnsafeMembership
        if n < offset @es @es'
            then n
            else n + prefixLenUnder @es @es'
{-# INLINE weakensUnderFor #-}
