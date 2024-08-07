{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.Shell where

import Control.Lens (makeLenses, makePrisms)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Effect.Concurrent.Pipe (
    Connection,
    Feed,
    OutPlumber,
    PipeComm,
    joinOutfluxToLeft,
    joinOutfluxToRight,
    swapOutflux,
 )
import Data.Effect.Foldl (Folding, FoldingH, FoldingMapH)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts (IsString)
import System.Exit (ExitCode)
import System.Posix.Types (GroupID, UserID)
import System.Process (StdStream)
import System.Process qualified as Raw

data CmdSpec
    = ShellCommand Text
    | RawCommand FilePath [Text]

data CreateProcess = CreateProcess
    { _cmdspec :: CmdSpec
    , _cwd :: Maybe FilePath
    , _env :: Maybe [(Text, Text)]
    , _closeFds :: Bool
    , _createGroup :: Bool
    , _delegateCtlc :: Bool
    , _detachConsole :: Bool
    , _createNewConsole :: Bool
    , _newSession :: Bool
    , _childGroup :: Maybe GroupID
    , _childUser :: Maybe UserID
    , _useProcessJobs :: Bool
    }

makePrisms ''CmdSpec
makeLenses ''CreateProcess

defaultCreateProcess :: CmdSpec -> CreateProcess
defaultCreateProcess cs =
    CreateProcess
        { _cmdspec = cs
        , _cwd = Nothing
        , _env = Nothing
        , _closeFds = False
        , _createGroup = False
        , _delegateCtlc = False
        , _detachConsole = False
        , _createNewConsole = False
        , _newSession = False
        , _childGroup = Nothing
        , _childUser = Nothing
        , _useProcessJobs = False
        }
{-# INLINE defaultCreateProcess #-}

data Process a where
    RunProcess :: CreateProcess -> Process ExitCode

makeEffectF [''Process]

proc :: FilePath -> [Text] -> CreateProcess
proc exe args = defaultCreateProcess $ RawCommand exe args
{-# INLINE proc #-}

shell :: Text -> CreateProcess
shell = defaultCreateProcess . ShellCommand
{-# INLINE shell #-}

toRawCreateProcess :: CreateProcess -> StdStream -> StdStream -> StdStream -> Raw.CreateProcess
toRawCreateProcess CreateProcess{..} stdIn stdOut stdErr =
    Raw.CreateProcess
        { cmdspec = toRawCmdSpec _cmdspec
        , cwd = _cwd
        , env = map (bimap T.unpack T.unpack) <$> _env
        , close_fds = _closeFds
        , create_group = _createGroup
        , delegate_ctlc = _delegateCtlc
        , detach_console = _detachConsole
        , create_new_console = _createNewConsole
        , new_session = _newSession
        , child_group = _childGroup
        , child_user = _childUser
        , use_process_jobs = _useProcessJobs
        , std_in = stdIn
        , std_out = stdOut
        , std_err = stdErr
        }
{-# INLINE toRawCreateProcess #-}

toRawCmdSpec :: CmdSpec -> Raw.CmdSpec
toRawCmdSpec = \case
    ShellCommand t -> Raw.ShellCommand $ T.unpack t
    RawCommand exe args -> Raw.RawCommand exe (map T.unpack args)
{-# INLINE toRawCmdSpec #-}

type Shell f =
    ( Process <: f
    , PipeComm (Connection Stdio) f
    , Folding Stdio <: f
    , FoldingMapH Stdio <<: f
    , FoldingH Stdio <<: f
    , Feed (Connection Stderr) <: f
    , OutPlumber Stderr Stdio <<: f
    )

newtype Stdio = Stdio {getStdio :: ByteString}
    deriving newtype (Eq, Ord, Semigroup, Monoid, IsString)
    deriving stock (Show)

newtype Stderr = Stderr {getStderr :: ByteString}
    deriving newtype (Eq, Ord, Semigroup, Monoid, IsString)
    deriving stock (Show)

stderrToOut :: OutPlumber Stderr Stdio <<: f => f a -> f a
stderrToOut = joinOutfluxToRight @Stderr @Stdio
{-# INLINE stderrToOut #-}

stdoutToErr :: OutPlumber Stderr Stdio <<: f => f a -> f a
stdoutToErr = joinOutfluxToLeft @Stderr @Stdio
{-# INLINE stdoutToErr #-}

swapStdOE :: OutPlumber Stderr Stdio <<: f => f a -> f a
swapStdOE = swapOutflux @Stderr @Stdio
{-# INLINE swapStdOE #-}
