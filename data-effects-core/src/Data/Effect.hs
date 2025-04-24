{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2024 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect where

import Data.Coerce (Coercible, coerce)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Kind (Type)

-- | The kind for effects.
type Effect = (Type -> Type) -> Type -> Type

-- | An order of effect.
data EffectOrder = FirstOrder | HigherOrder
    deriving (Show, Eq, Ord)

type family OrderOf (e :: Effect) :: EffectOrder

type family OrderCase (e :: EffectOrder) a b where
    OrderCase 'FirstOrder a b = a
    OrderCase 'HigherOrder a b = b

type family LabelOf (e :: Effect)

class
    ( OrderOf e ~ 'FirstOrder
    , forall f g a. Coercible (e f a) (e g a)
    ) =>
    FirstOrder (e :: Effect)

-- | A higher-order polynomial functor.
class PolyHFunctor e

-- * Nop Effect

-- | A effect with no operations.
data Nop :: Effect
    deriving anyclass (FirstOrder)

data NopLabel
type instance LabelOf Nop = NopLabel
type instance OrderOf Nop = 'FirstOrder
instance HFunctor Nop where
    hfmap _ = \case {}
    {-# INLINE hfmap #-}

-- * Embedding Effect

newtype Emb e (f :: Type -> Type) (a :: Type) = Emb {getEmb :: e a}
    deriving anyclass (FirstOrder)
    deriving newtype (Functor, Applicative, Monad, Foldable)
    deriving stock (Traversable)

data EmbLabel (e :: Type -> Type)
type instance LabelOf (Emb e) = EmbLabel e
type instance OrderOf (Emb e) = 'FirstOrder
instance HFunctor (Emb e) where
    hfmap _ = coerce
    {-# INLINE hfmap #-}

newtype Unemb e a = Unemb {getUnemb :: forall f. e f a}

-- * Reader Effects

-- | An effect that holds a value of type @r@ in the context (environment).
data Ask r :: Effect where
    -- | Obtain a value from the environment.
    Ask :: Ask r f r

data AskLabel
type instance LabelOf (Ask r) = AskLabel
type instance OrderOf (Ask r) = 'FirstOrder
instance FirstOrder (Ask r)
instance HFunctor (Ask r) where
    hfmap _ = coerce
    {-# INLINE hfmap #-}

-- | An effect that locally modifies the value held in the environment.
data Local r :: Effect where
    -- | Locally modifies the value held in the environment.
    Local
        :: (r -> r)
        -- ^ A function that transforms the original value to the modified value.
        -> f a
        -- ^ The local scope where the modification is applied.
        -> Local r f a

data LocalLabel
type instance LabelOf (Local r) = LocalLabel
type instance OrderOf (Local r) = 'HigherOrder
instance HFunctor (Local r) where
    hfmap phi (Local f a) = Local f (phi a)
    {-# INLINE hfmap #-}

-- * State Effect

-- | An effect for holding mutable state values in the context.
data State s :: Effect where
    -- | Retrieves the current state value from the context.
    Get :: State s f s
    -- | Overwrites the state value in the context.
    Put :: s -> State s f ()

data StateLabel
type instance LabelOf (State s) = StateLabel
type instance OrderOf (State s) = 'FirstOrder
instance FirstOrder (State s)
instance HFunctor (State s) where
    hfmap _ = coerce
    {-# INLINE hfmap #-}

-- * Writer Effects

-- | An effect that can accumulate values monoidally in a context.
data Tell w :: Effect where
    -- | Accumulates new values to the cumulative value held in the context.
    Tell :: w -> Tell w f ()

data TellLabel
type instance LabelOf (Tell w) = TellLabel
type instance OrderOf (Tell w) = 'FirstOrder
instance FirstOrder (Tell w)
instance HFunctor (Tell w) where
    hfmap _ = coerce
    {-# INLINE hfmap #-}

-- | An effect that performs local operations on accumulations in the context on a per-scope basis.
data WriterH w :: Effect where
    -- | Obtains the accumulated value in the scope and returns it together as a pair.
    Listen
        :: f a
        -- ^ The scope from which to obtain the accumulation.
        -> WriterH w f (w, a)
    -- | Modifies the accumulation in the scope based on the given function.
    Censor
        :: (w -> w)
        -- ^ A function for modifying the accumulated value.
        -> f a
        -- ^ The scope where the modification is applied.
        -> WriterH w f a

data WriterHLabel
type instance LabelOf (WriterH w) = WriterHLabel
type instance OrderOf (WriterH w) = 'HigherOrder
instance HFunctor (WriterH w) where
    hfmap phi = \case
        Listen a -> Listen $ phi a
        Censor f a -> Censor f (phi a)
    {-# INLINE hfmap #-}

-- * Exception Effects

-- | An effect to escape from the normal control structure with an exception value of type @e@ in the middle of a context.
data Throw e :: Effect where
    -- | Throws an exception; that is, escapes from the normal control structure with an exception value in the middle of a context.
    Throw :: e -> Throw e f a

data ThrowLabel
type instance LabelOf (Throw e) = ThrowLabel
type instance OrderOf (Throw e) = 'FirstOrder
instance FirstOrder (Throw e)
instance HFunctor (Throw e) where
    hfmap _ = coerce
    {-# INLINE hfmap #-}

-- | An effect to catch exceptions.
data Catch e :: Effect where
    -- | Catches exceptions within a scope and processes them according to the given exception handler.
    Catch
        :: f a
        -- ^ The scope in which to catch exceptions.
        -> (e -> f a)
        -- ^ Exception handler. Defines the processing to perform when an exception is thrown within the scope.
        -> Catch e f a

data CatchLabel
type instance LabelOf (Catch w) = CatchLabel
type instance OrderOf (Catch w) = 'HigherOrder
instance HFunctor (Catch w) where
    hfmap phi (Catch a hdl) = Catch (phi a) (phi . hdl)
    {-# INLINE hfmap #-}

-- * Non-Determinism Effects

-- | An effect that eliminates a branch by causing the current branch context of a non-deterministic computation to fail.
data Empty :: Effect where
    -- | Eliminates a branch by causing the current branch context of a non-deterministic computation to fail.
    Empty :: Empty f a

data EmptyLabel
type instance LabelOf Empty = EmptyLabel
type instance OrderOf Empty = 'FirstOrder
instance FirstOrder Empty
instance HFunctor Empty where
    hfmap _ = coerce
    {-# INLINE hfmap #-}

-- | An effect that splits the computation into two branches.
data Choose :: Effect where
    -- | Splits the computation into two branches.
    -- As a result of executing @choose@, the world branches into one where `False` is returned and one where `True` is returned.
    Choose :: Choose f Bool

data ChooseLabel
type instance LabelOf Choose = ChooseLabel
type instance OrderOf Choose = 'FirstOrder
instance FirstOrder Choose
instance HFunctor Choose where
    hfmap _ = coerce
    {-# INLINE hfmap #-}

{- |
An effect that executes two branches as scopes.
A higher-order version of the t`Choose` effect.
-}
data ChooseH :: Effect where
    -- | Executes the given two scopes as branches.
    -- Even if one fails due to the `empty` operation, the whole does not fail as long as the other does not fail.
    ChooseH :: f a -> f a -> ChooseH f a

data ChooseHLabel
type instance LabelOf ChooseH = ChooseHLabel
type instance OrderOf ChooseH = 'HigherOrder
instance HFunctor ChooseH where
    hfmap phi (ChooseH a b) = ChooseH (phi a) (phi b)
    {-# INLINE hfmap #-}

-- * Fail Effect

data Fail :: Effect where
    Fail :: String -> Fail f a

data FailLabel
type instance LabelOf Fail = FailLabel
type instance OrderOf Fail = 'FirstOrder
instance FirstOrder Fail
instance HFunctor Fail where
    hfmap _ = coerce
    {-# INLINE hfmap #-}

-- * Fix Effect

data Fix :: Effect where
    Efix :: (a -> f a) -> Fix f a

data FixLabel
type instance LabelOf Fix = FixLabel
type instance OrderOf Fix = 'HigherOrder
instance HFunctor Fix where
    hfmap phi (Efix f) = Efix $ phi . f
    {-# INLINE hfmap #-}

-- * Unlift Effect

data UnliftBase b f (a :: Type) where
    WithRunInBase :: ((forall x. f x -> b x) -> b a) -> UnliftBase b f a

type UnliftIO = UnliftBase IO

data UnliftBaseLabel (b :: Type -> Type)
type instance LabelOf (UnliftBase b) = UnliftBaseLabel b
type instance OrderOf (UnliftBase b) = 'HigherOrder
instance HFunctor (UnliftBase b) where
    hfmap phi (WithRunInBase f) = WithRunInBase \run -> f $ run . phi
    {-# INLINE hfmap #-}

-- * CallCC Effect (Sub/Jump-based)

data CC ref :: Effect where
    SubFork :: CC ref f (Either (ref a) a)
    Jump :: ref a -> a -> CC ref f b

data CCLabel
type instance LabelOf (CC ref) = CCLabel
type instance OrderOf (CC ref) = 'FirstOrder
instance FirstOrder (CC ref)
instance HFunctor (CC ref) where
    hfmap _ = coerce
    {-# INLINE hfmap #-}
