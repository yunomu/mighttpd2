{-# LANGUAGE DoAndIfThenElse #-}

module Log.SingleFile where

import Control.Monad
import Control.Concurrent
import Log.Apache
import Log.Buffer
import Log.Clock
import Log.File
import Log.Queue
import Log.Types
import Network.Wai.Application.Classic
import System.Exit
import System.Posix

type MemFile = Buffer

singleFileLoggerInit :: FileLogSpec -> IO (Logger, IO ())
singleFileLoggerInit spec = do
    tmref <- clockInit
    logQ <- queueInit
    memfile <- openLogFile (log_file spec) (log_file_size spec)
    mvar <- newMVar memfile
    let closer = Catch $ singleFileLoggerFinalizer spec mvar
    installHandler sigTERM closer Nothing
    installHandler sigINT  closer Nothing
    return (mightyLogger (tmref,logQ), singleFileLogger spec logQ mvar)

singleFileLogger :: FileLogSpec -> LogQ -> MVar Buffer -> IO ()
singleFileLogger spec logQ mvar = forever $ do
    bss <- dequeue logQ
    memfile <- takeMVar mvar
    mbuf' <- copyByteStrings memfile bss
    case mbuf' of
        Nothing -> do
            closeLogFile (log_file spec) memfile
            rotate (log_file spec) (log_backup_number spec)
            buf' <- openLogFile (log_file spec) (log_file_size spec)
            Just buf'' <- copyByteStrings buf' bss
            putMVar mvar buf''
        Just buf' -> putMVar mvar buf'

singleFileLoggerFinalizer :: FileLogSpec -> MVar Buffer -> IO ()
singleFileLoggerFinalizer spec mvar = do
    memfile <- takeMVar mvar
    closeLogFile (log_file spec) memfile
    exitImmediately ExitSuccess
