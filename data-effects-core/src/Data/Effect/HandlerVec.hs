{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-License-Identifier: MPL-2.0

module Data.Effect.HandlerVec (
    module Data.Effect.HandlerVec,
    module Data.Effect.HandlerVec.Rec,
)
where

import Data.Coerce (Coercible, coerce)
import Data.Effect (Effect, EffectOrder (FirstOrder, HigherOrder), Emb, FirstOrder, LabelOf, OrderCase, OrderOf)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.HandlerVec.Rec (
    At,
    Each,
    KnownList,
    Membership (UnsafeMembership),
    Rec,
    Suffix,
    SuffixUnder,
    cons,
    index,
    intVal,
    unMembership,
    update,
    update0,
    weakenFor,
    pattern Here,
    type (++),
 )
import Data.Effect.HandlerVec.Rec qualified as Rec
import Data.Effect.Tag (type (#))
import Data.Functor.Const (getConst)
import Data.Kind (Constraint, Type)
import Data.Void (absurd)
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeLits (Symbol)
import GHC.TypeNats (KnownNat, type (+))
import Unsafe.Coerce (unsafeCoerce)

newtype HandlerVec es f r
    = HandlerVec
    { unHandlerVec
        :: forall g
         . (forall x. g x -> f x)
        -> Rec (Handler g r) es
    }

coerceFOEs :: (FOEs es) => HandlerVec es f r -> HandlerVec es g r
coerceFOEs = unsafeCoerce
{-# INLINE coerceFOEs #-}

hcfmapVec :: (forall x. f x -> g x) -> HandlerVec es g r -> HandlerVec es f r
hcfmapVec phi (HandlerVec f) = HandlerVec \kk -> f $ phi . kk
{-# INLINE hcfmapVec #-}

-- | /O(n)/ where /n/ = @length es@
vmapVec :: (forall x. r x -> r' x) -> HandlerVec es g r -> HandlerVec es g r'
vmapVec f = transVec $ Rec.map \(Handler h) -> Handler $ f . h
{-# INLINE vmapVec #-}

hfmapCoerceVec :: (Coercible r r') => HandlerVec es g r -> HandlerVec es g r'
hfmapCoerceVec = coerce

hcfmapCoerceVec :: (Coercible f g) => HandlerVec es g r -> HandlerVec es f r
hcfmapCoerceVec = coerce

suffix :: forall es es' g r. (Suffix es es') => HandlerVec es' g r -> HandlerVec es g r
suffix = transVec Rec.suffix
{-# INLINE suffix #-}

suffixUnder :: forall e es es' g r. (Suffix es es') => HandlerVec (e ': es') g r -> HandlerVec (e ': es) g r
suffixUnder = transVec Rec.suffixUnder
{-# INLINE suffixUnder #-}

suffixUnders :: forall es es' g r. (SuffixUnder es es') => HandlerVec es' g r -> HandlerVec es g r
suffixUnders = transVec Rec.suffixUnders
{-# INLINE suffixUnders #-}

removeUnder :: forall e es es' g r. (KnownList es) => HandlerVec (e ': es ++ es') g r -> HandlerVec (e ': es') g r
removeUnder = transVec $ Rec.removeUnder @_ @es
{-# INLINE removeUnder #-}

removeUnders
    :: forall es es' es'' g r
     . (KnownList es, KnownList es')
    => HandlerVec (es ++ es' ++ es'') g r
    -> HandlerVec (es ++ es'') g r
removeUnders = transVec $ Rec.removeUnders @es @es' @es''
{-# INLINE removeUnders #-}

drop :: forall es es' g r. (KnownList es) => HandlerVec (es ++ es') g r -> HandlerVec es' g r
drop = transVec $ Rec.drop @es
{-# INLINE drop #-}

take :: forall es' es g r. (KnownList es) => HandlerVec (es ++ es') g r -> HandlerVec es g r
take = transVec $ Rec.take @es'
{-# INLINE take #-}

drop1 :: forall fs x es' g r. (KnownList fs) => HandlerVec (Each fs x ++ es') g r -> HandlerVec es' g r
drop1 = transVec $ Rec.drop1 @fs @x
{-# INLINE drop1 #-}

take1 :: forall fs x es' g r. (KnownList fs) => HandlerVec (Each fs x ++ es') g r -> HandlerVec (Each fs x) g r
take1 = transVec $ Rec.take1 @fs @x @es'
{-# INLINE take1 #-}

transVec :: (forall g. Rec (Handler g r) es -> Rec (Handler g r') es') -> HandlerVec es f r -> HandlerVec es' f r'
transVec f (HandlerVec g) = HandlerVec \kk -> f $ g kk
{-# INLINE transVec #-}

concat :: forall es es' f r. HandlerVec es f r -> HandlerVec es' f r -> HandlerVec (es ++ es') f r
concat (HandlerVec f) (HandlerVec g) = HandlerVec \kk -> f kk `Rec.concat` g kk
{-# INLINE concat #-}

newtype Handler f r (e :: Effect)
    = Handler {getHandler :: forall x. e f x -> r x}

type KnownOrder e = Elem e (OrderOf e)

-- todo: replace `KnownOrder` use with just `HFunctor` and bench
class (order ~ OrderOf e) => Elem e order where
    hfmapElem :: (forall x. f x -> g x) -> e f a -> e g a

instance (FirstOrder e) => Elem e 'FirstOrder where
    hfmapElem _ = coerce
    {-# INLINE hfmapElem #-}

instance (OrderOf e ~ 'HigherOrder, HFunctor e) => Elem e 'HigherOrder where
    hfmapElem = hfmap
    {-# INLINE hfmapElem #-}

handlerFor :: Membership e es -> HandlerVec es f r -> (forall x. e f x -> r x)
handlerFor i (HandlerVec v) = getHandler $ index i (v id)
{-# INLINE handlerFor #-}

infixr 5 !:
(!:) :: (KnownOrder e) => (forall x. e f x -> r x) -> HandlerVec es f r -> HandlerVec (e ': es) f r
h !: HandlerVec hs = HandlerVec \kk -> cons (Handler $ h . hfmapElem kk) (hs kk)
{-# INLINE (!:) #-}

generate
    :: (FOEs es)
    => HandlerVec es g r'
    -> (forall e x. Membership e es -> e h x -> r x)
    -> HandlerVec es f r
generate (HandlerVec v) h = HandlerVec \_ -> Rec.generate (v $ absurd . getConst) \i -> Handler $ h i . unsafeCoerce
{-# INLINE generate #-}

override0 :: (KnownOrder e') => (forall x. e' f x -> r x) -> HandlerVec (e ': es) f r -> HandlerVec (e' ': es) f r
override0 h (HandlerVec f) = HandlerVec \kk -> update0 (Handler $ h . hfmapElem kk) (f kk)
{-# INLINE override0 #-}

overrideFor :: (KnownOrder e) => Membership e es -> (forall x. e f x -> r x) -> HandlerVec es f r -> HandlerVec es f r
overrideFor i h (HandlerVec f) = HandlerVec \kk -> update i (Handler $ h . hfmapElem kk) (f kk)
{-# INLINE overrideFor #-}

hcfmapHandler :: (KnownOrder e) => (forall x. f x -> g x) -> Handler g r e -> Handler f r e
hcfmapHandler phi (Handler f) = Handler \v -> f $ hfmapElem phi v

empty :: HandlerVec '[] f r
empty = HandlerVec \_ -> Rec.nil
{-# INLINE empty #-}

singleton :: (KnownOrder e) => (forall x. e f x -> r x) -> HandlerVec '[e] f r
singleton h = HandlerVec \kk -> Rec.singleton $ Handler $ h . hfmapElem kk
{-# INLINE singleton #-}

class FOEs es
instance FOEs '[]
instance (FirstOrder e, FOEs es) => FOEs (e ': es)

data LabelResolver
data KeyResolver
data IdentityResolver

data KeyDiscriminator key
data NoKeyDiscriminator

data IdentityDiscriminator (e :: Effect)

type family Discriminator resolver (e :: Effect)
type instance Discriminator LabelResolver e = LabelOf e
type instance Discriminator KeyResolver e = KeyOf e
type instance Discriminator IdentityResolver e = IdentityDiscriminator e

type family KeyOf e where
    KeyOf (e # key) = KeyDiscriminator key
    KeyOf e = NoKeyDiscriminator

type family ResolverName resolver :: Symbol
type instance ResolverName LabelResolver = "label"
type instance ResolverName KeyResolver = "key"
type instance ResolverName IdentityResolver = "identity"

infix 4 :>
infix 4 `In`

type e :> es = MemberBy LabelResolver (Discriminator LabelResolver e) e es
type Has key e es = MemberBy KeyResolver (KeyDiscriminator key) (e # key) es
type e `In` es = MemberBy IdentityResolver (IdentityDiscriminator e) e es
type KnownIndex i es = (KnownNat i, KnownOrder (At i es))

type MemberBy resolver dscr e es =
    ( FindBy resolver dscr (Discriminator resolver (HeadOf es)) e es
    , ErrorIfNotFound resolver dscr (Discriminator resolver (HeadOf es)) e es es
    , KnownOrder e
    )

class
    (dscr ~ Discriminator resolver e, dscr' ~ Discriminator resolver (HeadOf r)) =>
    FindBy resolver dscr dscr' e r
        | resolver r -> e
    where
    findBy :: Membership e r

instance
    (dscr ~ Discriminator resolver e, dscr ~ Discriminator resolver e', e ~ e')
    => FindBy resolver dscr dscr e (e' ': r)
    where
    findBy = Here
    {-# INLINE findBy #-}

instance
    {-# OVERLAPPABLE #-}
    ( dscr ~ Discriminator resolver e
    , dscr' ~ Discriminator resolver e'
    , FindBy resolver dscr (Discriminator resolver (HeadOf r)) e r
    )
    => FindBy resolver dscr dscr' e (e' ': r)
    where
    findBy = weakenFor $ findBy @resolver @dscr @(Discriminator resolver (HeadOf r)) @e @r
    {-# INLINE findBy #-}

membership
    :: forall resolver dscr e es
     . (FindBy resolver dscr (Discriminator resolver (HeadOf es)) e es)
    => Membership e es
membership = findBy @resolver @dscr @(Discriminator resolver (HeadOf es)) @e @es
{-# INLINE membership #-}

labelMembership
    :: forall e es
     . (FindBy LabelResolver (LabelOf e) (LabelOf (HeadOf es)) e es)
    => Membership e es
labelMembership = membership @LabelResolver
{-# INLINE labelMembership #-}

keyMembership
    :: forall key e es
     . (FindBy KeyResolver (KeyDiscriminator key) (KeyOf (HeadOf es)) (e # key) es)
    => Membership (e # key) es
keyMembership = membership @KeyResolver
{-# INLINE keyMembership #-}

identityMembership
    :: forall e es
     . (FindBy IdentityResolver (IdentityDiscriminator e) (IdentityDiscriminator (HeadOf es)) e es)
    => Membership e es
identityMembership = membership @IdentityResolver
{-# INLINE identityMembership #-}

class
    (dscr ~ Discriminator resolver e, dscr' ~ Discriminator resolver (HeadOf r)) =>
    ErrorIfNotFound resolver dscr dscr' (e :: Effect) (r :: [Effect]) (w :: [Effect])

instance
    ( TypeError
        ( 'Text "The effect ‘"
            ':<>: 'ShowType e
            ':<>: 'Text "’ does not exist within the effect list"
            ':$$: 'Text "  ‘"
            ':<>: 'ShowType w
            ':<>: 'Text "’"
            ':$$: 'Text "Resolver: "
            ':<>: 'Text (ResolverName resolver)
            ':$$: 'Text "Discriminator: "
            ':<>: 'ShowType dscr
        )
    , dscr ~ Discriminator resolver e
    , dscr' ~ Discriminator resolver (HeadOf '[])
    )
    => ErrorIfNotFound resolver dscr dscr' e '[] w

instance
    (dscr ~ Discriminator resolver e, dscr ~ Discriminator resolver e', e ~ e')
    => ErrorIfNotFound resolver dscr dscr e (e' ': r) w

instance
    {-# OVERLAPPABLE #-}
    ( dscr ~ Discriminator resolver e
    , dscr' ~ Discriminator resolver e'
    , ErrorIfNotFound resolver dscr (Discriminator resolver (HeadOf r)) e r w
    )
    => ErrorIfNotFound resolver dscr dscr' e (e' ': r) w

instance
    {-# INCOHERENT #-}
    ( dscr ~ Discriminator resolver e
    , dscr' ~ Discriminator resolver (HeadOf r)
    )
    => ErrorIfNotFound resolver dscr dscr' e r w

type family HeadOf es where
    HeadOf (e ': es) = e

type family RemoveHOEs (es :: [Effect]) where
    RemoveHOEs '[] = '[]
    RemoveHOEs (e ': es) =
        OrderCase (OrderOf e) (e ': RemoveHOEs es) (RemoveHOEs es)

type RemoveH es = RemoveH_ es 0 (OrderOfHead es)

type family OrderOfHead es where
    OrderOfHead (e ': es) = OrderOf e

class (FOEs (RemoveHOEs es), orderOfHead ~ OrderOfHead es) => RemoveH_ es i orderOfHead where
    weakenHFor :: Membership e (RemoveHOEs es) -> Membership e es

instance (OrderOfHead '[] ~ orderOfHead) => RemoveH_ '[] i orderOfHead where
    weakenHFor = id
    {-# INLINE weakenHFor #-}

instance (FirstOrder e, RemoveH_ es (i + 1) _orderOfHead) => RemoveH_ (e ': es) i 'FirstOrder where
    weakenHFor = coerce $ weakenHFor @es @(i + 1)
    {-# INLINE weakenHFor #-}

instance
    (OrderOf e ~ 'HigherOrder, RemoveH_ es (i + 1) _orderOfHead, FOEs (RemoveHOEs es), KnownNat i)
    => RemoveH_ (e ': es) i 'HigherOrder
    where
    weakenHFor (UnsafeMembership n) =
        UnsafeMembership $
            if n < intVal @i
                then n
                else unMembership (weakenHFor @es @(i + 1) $ UnsafeMembership n) + 1
    {-# INLINE weakenHFor #-}

type KnownLength :: forall {k}. [k] -> Constraint
class KnownLength xs where
    reifyLength :: Int

instance KnownLength '[] where
    reifyLength = 0
    {-# INLINE reifyLength #-}

instance (KnownLength xs) => KnownLength (x ': xs) where
    reifyLength = 1 + reifyLength @xs
    {-# INLINE reifyLength #-}

splitFor
    :: forall es es' e r
     . (KnownLength es)
    => (Membership e es -> r)
    -> (Membership e es' -> r)
    -> Membership e (es ++ es')
    -> r
splitFor f g (UnsafeMembership n)
    | n < l = f $ UnsafeMembership n
    | otherwise = g $ UnsafeMembership $ n - l
  where
    l = reifyLength @es
{-# INLINE splitFor #-}

suffixFor :: forall es' es e. Membership e es -> Membership e (es ++ es')
suffixFor = coerce
{-# INLINE suffixFor #-}

prefixFor :: forall es' es e. (KnownLength es') => Membership e es -> Membership e (es' ++ es)
prefixFor (UnsafeMembership n) = UnsafeMembership $ reifyLength @es' + n
{-# INLINE prefixFor #-}

suffixFor1 :: forall fs x es e. Membership e es -> Membership e (es ++ Each fs x)
suffixFor1 = coerce
{-# INLINE suffixFor1 #-}

prefixFor1 :: forall fs x es e. (KnownLength fs) => Membership e es -> Membership e (Each fs x ++ es)
prefixFor1 (UnsafeMembership n) = UnsafeMembership $ reifyLength @fs + n
{-# INLINE prefixFor1 #-}
