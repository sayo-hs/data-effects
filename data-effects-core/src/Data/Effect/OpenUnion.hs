{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.OpenUnion where

import Data.Data (Proxy (Proxy))
import Data.Effect (Effect, EffectOrder (FirstOrder, HigherOrder), FirstOrder, LabelOf, OrderCase, OrderOf)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.Key (type (#>))
import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), KnownNat, TypeError, natVal, type (+), type (-))
import Unsafe.Coerce (unsafeCoerce)

data Union (es :: [Effect]) (f :: Type -> Type) (a :: Type) where
    UnsafeUnion
        :: {-# UNPACK #-} !Word
        -- ^ A natural number tag to identify the element of the union.
        -> e g a
        -> {-# UNPACK #-} !EffectOrder
        -> (forall x. g x -> f x)
        -> Union es f a

instance HFunctor (Union es) where
    hfmap phi u@(UnsafeUnion n e order koi) =
        case order of
            FirstOrder -> unsafeCoerce u
            HigherOrder -> UnsafeUnion n e HigherOrder (phi . koi)
    {-# INLINE hfmap #-}

class FOEs es
instance FOEs '[]
instance (FirstOrder e, FOEs es) => FOEs (e ': es)

coerceFOEs :: (FOEs es) => Union es f a -> Union es g a
coerceFOEs = unsafeCoerce
{-# INLINE coerceFOEs #-}

type family RemoveHOEs (es :: [Effect]) where
    RemoveHOEs '[] = '[]
    RemoveHOEs (e ': es) =
        OrderCase (OrderOf e) (e ': RemoveHOEs es) (RemoveHOEs es)

type KnownOrders es = WeakenH es 0 (OrderOfHead es)

type family OrderOfHead es where
    OrderOfHead (e ': es) = OrderOf e

class (FOEs (RemoveHOEs es), orderOfHead ~ OrderOfHead es) => WeakenH es i orderOfHead where
    weakenH :: Word -> Word

instance (OrderOfHead '[] ~ orderOfHead) => WeakenH '[] i orderOfHead where
    weakenH = id
    {-# INLINE weakenH #-}

instance (FirstOrder e, WeakenH es (i + 1) _orderOfHead) => WeakenH (e ': es) i 'FirstOrder where
    weakenH = weakenH @es @(i + 1)
    {-# INLINE weakenH #-}

instance
    (OrderOf e ~ 'HigherOrder, WeakenH es (i + 1) _orderOfHead, FOEs (RemoveHOEs es), KnownNat i)
    => WeakenH (e ': es) i 'HigherOrder
    where
    weakenH n = if n < wordVal @i then n else weakenH @es @(i + 1) n + 1
    {-# INLINE weakenH #-}

raiseH :: forall es f a. (KnownOrders es) => Union (RemoveHOEs es) f a -> Union es f a
raiseH = unsafeTransformUnion $ weakenH @es @0
{-# INLINE raiseH #-}

type e :> es = (Member (LabelIndex e es) e es, KnownOrder e)
type Has key e es = (Member (KeyIndex key es) e es, KnownOrder e)
type e `In` es = (Member (Index e es) e es, KnownOrder e)

type family ElemAt i es :: Effect where
    ElemAt 0 (e ': es) = e
    ElemAt i (e ': es) = ElemAt (i - 1) es
    ElemAt i '[] = TypeError ('Text "Effect index out of range")

type LabelIndex e es = FindByLabel (LabelOf e) es es

type family FindByLabel label es w where
    FindByLabel label (e ': es) w = CmpLabel (LabelOf e) label 0 (FindByLabel label es w + 1)
    FindByLabel label '[] w = TypeError ('Text "No the label" :<>: 'ShowType label :<>: 'Text " in " :<>: 'ShowType w)

type family CmpLabel (label :: Type) label' eq neq where
    CmpLabel l l eq neq = eq
    CmpLabel l l' eq neq = neq

type KeyIndex key es = FindByKey key es es

-- fixme: add uniqueness check
type family FindByKey key es w where
    FindByKey key (key #> e ': es) w = 0
    FindByKey key (e ': es) w = FindByKey key es w + 1
    FindByKey key '[] w = TypeError ('Text "No the key" :<>: 'ShowType key :<>: 'Text " in " :<>: 'ShowType w)

type Index e es = Find e es es

type family Find e (es :: [Effect]) w where
    Find e (e ': es) w = 0
    Find e (e' ': es) w = 1 + Find e es w
    Find e '[] w = TypeError ('Text "No " :<>: 'ShowType e :<>: 'Text " in " :<>: 'ShowType w)

elemIndex :: forall i e es. (Member i e es) => Word
elemIndex = wordVal @i
{-# INLINE elemIndex #-}

wordVal :: forall n. (KnownNat n) => Word
wordVal = fromIntegral $ natVal @n Proxy
{-# INLINE wordVal #-}

type KnownOrder e = Elem e (OrderOf e)

class (order ~ OrderOf e) => Elem e order where
    inject :: (Member i e es) => Proxy i -> e f a -> Union es f a
    project :: (Member i e es, HFunctor e) => Proxy i -> Union es f a -> Maybe (e f a)
    (!+) :: (HFunctor e) => (e f a -> r) -> (Union es f a -> r) -> Union (e ': es) f a -> r
    extract :: (HFunctor e) => Union '[e] f a -> e f a

    infixr 5 !+

type Member i e es = (KnownNat i, ElemAt i es ~ e)

inj :: forall i e es f a. (KnownOrder e, Member i e es) => e f a -> Union es f a
inj = inject $ Proxy @i
{-# INLINE inj #-}

prj :: forall i e es f a. (KnownOrder e, Member i e es, HFunctor e) => Union es f a -> Maybe (e f a)
prj = project $ Proxy @i
{-# INLINE prj #-}

decomp :: (KnownOrder e, HFunctor e) => Union (e ': es) f a -> Either (e f a) (Union es f a)
decomp = Left !+ Right
{-# INLINE decomp #-}

instance (FirstOrder e) => Elem e 'FirstOrder where
    inject :: forall i es f a. (Member i e es) => Proxy i -> e f a -> Union es f a
    inject _ e = UnsafeUnion (elemIndex @i @e @es) e FirstOrder undefined

    project
        :: forall i es f a
         . (Member i e es, HFunctor e)
        => Proxy i
        -> Union es f a
        -> Maybe (e f a)
    project _ (UnsafeUnion n e _ _) =
        if n == elemIndex @i @e @es
            then Just $ unsafeCoerce e
            else Nothing

    (f !+ g) (UnsafeUnion n e koi order) =
        if n == 0
            then f $ unsafeCoerce e
            else g $ UnsafeUnion (n - 1) e koi order

    extract (UnsafeUnion _ e _ _) = unsafeCoerce e

    {-# INLINE inject #-}
    {-# INLINE project #-}
    {-# INLINE (!+) #-}
    {-# INLINE extract #-}

instance (OrderOf e ~ 'HigherOrder) => Elem e 'HigherOrder where
    inject :: forall i es f a. (Member i e es) => Proxy i -> e f a -> Union es f a
    inject _ e = UnsafeUnion (elemIndex @i @e @es) e HigherOrder id

    project :: forall i es f a. (Member i e es, HFunctor e) => Proxy i -> Union es f a -> Maybe (e f a)
    project _ (UnsafeUnion n e _ koi) =
        if n == elemIndex @i @e @es
            then Just $ hfmap koi (unsafeCoerce e)
            else Nothing

    (f !+ g) (UnsafeUnion n e _ koi) =
        if n == 0
            then f $ hfmap koi (unsafeCoerce e)
            else g $ UnsafeUnion (n - 1) e HigherOrder koi

    extract (UnsafeUnion _ e _ koi) = hfmap koi (unsafeCoerce e)

    {-# INLINE inject #-}
    {-# INLINE project #-}
    {-# INLINE (!+) #-}
    {-# INLINE extract #-}

projectAnyOE :: forall i e es f a. (Member i e es, HFunctor e) => Union es f a -> Maybe (e f a)
projectAnyOE (UnsafeUnion n e order koi) =
    if n == elemIndex @i @e @es
        then Just $ hfmapDynUnsafeCoerce order koi e
        else Nothing

infixr 5 `caseAnyOE`

caseAnyOE :: (HFunctor e) => (e f a -> r) -> (Union es f a -> r) -> Union (e ': es) f a -> r
(f `caseAnyOE` g) (UnsafeUnion n e order koi) =
    if n == 0
        then f $ hfmapDynUnsafeCoerce order koi e
        else g $ UnsafeUnion (n - 1) e order koi

extractAnyOE :: (HFunctor e) => Union es f a -> e f a
extractAnyOE (UnsafeUnion _ e order koi) = hfmapDynUnsafeCoerce order koi e

{-# INLINE projectAnyOE #-}
{-# INLINE caseAnyOE #-}
{-# INLINE extractAnyOE #-}

hfmapDynUnsafeCoerce :: (HFunctor e') => EffectOrder -> (forall x. f x -> g x) -> e f a -> e' g a
hfmapDynUnsafeCoerce order phi e = case order of
    FirstOrder -> unsafeCoerce e
    HigherOrder -> hfmap phi (unsafeCoerce e)
{-# INLINE hfmapDynUnsafeCoerce #-}

nil :: Union '[] f a -> r
nil _ = error "Effect system internal error: nil - An empty effect union, which should not be possible to create, has been created."

unsafeTransformUnion :: forall es es' f a. (Word -> Word) -> Union es f a -> Union es' f a
unsafeTransformUnion f (UnsafeUnion n e order koi) = UnsafeUnion (f n) e order koi
{-# INLINE unsafeTransformUnion #-}

class Weaken (es :: [Effect]) (es' :: [Effect]) where
    prefixLen :: Word

instance {-# INCOHERENT #-} Weaken es es where
    prefixLen = 0
    {-# INLINE prefixLen #-}

instance (Weaken es es') => Weaken es (e ': es') where
    prefixLen = prefixLen @es @es' + 1
    {-# INLINE prefixLen #-}

weaken :: forall es es' f a. (Weaken es es') => Union es f a -> Union es' f a
weaken = unsafeTransformUnion (+ prefixLen @es @es')
{-# INLINE weaken #-}

class WeakenUnder (es :: [Effect]) (es' :: [Effect]) where
    prefixLenUnder :: Word
    offset :: Word

instance {-# INCOHERENT #-} (Weaken es es') => WeakenUnder es es' where
    prefixLenUnder = prefixLen @es @es'
    offset = 0

    {-# INLINE prefixLenUnder #-}
    {-# INLINE offset #-}

instance (WeakenUnder es es') => WeakenUnder (e ': es) (e ': es') where
    prefixLenUnder = prefixLenUnder @es @es'
    offset = offset @es @es' + 1

    {-# INLINE prefixLenUnder #-}
    {-# INLINE offset #-}

weakenUnder :: forall es es' f a. (WeakenUnder es es') => Union es f a -> Union es' f a
weakenUnder = unsafeTransformUnion \n ->
    if n < offset @es @es'
        then n
        else n + prefixLenUnder @es @es'
{-# INLINE weakenUnder #-}
