{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

An effect to escape from the normal control structure with an exception value in the middle of a context.
-}
module Data.Effect.Except where

import Data.Effect (Emb)
import Data.Effect.Unlift (UnliftIO)
import UnliftIO (Exception, throwIO)
import UnliftIO qualified as IO

-- | An effect to escape from the normal control structure with an exception value of type @e@ in the middle of a context.
data Throw e :: Effect where
    -- | Throws an exception; that is, escapes from the normal control structure with an exception value in the middle of a context.
    Throw :: e -> Throw e f a

-- | An effect to catch exceptions.
data Catch e :: Effect where
    -- | Catches exceptions within a scope and processes them according to the given exception handler.
    Catch
        :: f a
        -- ^ The scope in which to catch exceptions.
        -> (e -> f a)
        -- ^ Exception handler. Defines the processing to perform when an exception is thrown within the scope.
        -> Catch e f a

makeEffectF ''Throw
makeEffectH ''Catch

-- | Throws the given `Either` value as an exception if it is `Left`.
liftEither :: (Throw e :> es, Applicative (Eff ff es), Free c ff) => Either e a -> Eff ff es a
liftEither = either throw pure
{-# INLINE liftEither #-}

-- | Throws the result of the given action as an exception if it is `Left`.
joinEither :: (Throw e :> es, Monad (Eff ff es), Free c ff) => Eff ff es (Either e a) -> Eff ff es a
joinEither = (>>= either throw pure)
{-# INLINE joinEither #-}

-- | If the given `Either` value is `Left`, execute it as an action.
joinExcept :: (Monad m) => Either (m a) a -> m a
joinExcept = either id pure
{-# INLINE joinExcept #-}

-- | If the result of the given action is `Left`, execute it as an action.
exc :: (Monad m) => m (Either (m a) a) -> m a
exc = (>>= either id pure)
{-# INLINE exc #-}

-- | If an exception occurs, executes the given exception handler, but the exception is not stopped there and is rethrown.
withExcept
    :: (Catch e :> es, Throw e :> es, Applicative (Eff ff es), Free c ff)
    => Eff ff es a
    -- ^ Scope to which the exception handler applies
    -> (e -> Eff ff es ())
    -- ^ Exception handler
    -> Eff ff es a
withExcept thing after = thing `catch` \e -> after e *> throw e
{-# INLINE withExcept #-}

-- | If an exception occurs, executes the specified action, but the exception is not stopped there and is rethrown.
onExcept
    :: forall e es ff a c
     . (Catch e :> es, Throw e :> es, Applicative (Eff ff es), Free c ff)
    => Eff ff es a
    -- ^ Scope in which to detect exceptions
    -> Eff ff es ()
    -- ^ Action to execute in case of an exception
    -> Eff ff es a
onExcept thing after = thing `withExcept` \(_ :: e) -> after
{-# INLINE onExcept #-}

-- | Interpret the t'Throw' effect based on an IO-fused semantics using IO-level exceptions.
runThrowIO
    :: forall e es ff a c
     . (Emb IO `In` es, Exception e, Monad (Eff ff es), Free c ff)
    => Eff ff (Throw e ': es) a
    -> Eff ff es a
runThrowIO = interpret \(Throw e) -> throwIO e
{-# INLINE runThrowIO #-}

-- | Interpret the t'Catch' effect based on an IO-fused semantics using IO-level exceptions.
runCatchIO
    :: forall e es ff a c
     . (UnliftIO :> es, Emb IO `In` es, Exception e, Monad (Eff ff es), Free c ff)
    => Eff ff (Catch e ': es) a
    -> Eff ff es a
runCatchIO = interpret \(Catch action hdl) -> IO.catch action hdl
{-# INLINE runCatchIO #-}
