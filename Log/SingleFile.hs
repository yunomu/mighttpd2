{-# LANGUAGE DoAndIfThenElse #-}

module Log.SingleFile where

import Control.Applicative
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
import System.Timeout

data FileBuffer = FileBuffer Fd Buffer

singleFileLoggerInit :: FileLogSpec -> IO (Logger, IO ())
singleFileLoggerInit spec = do
    tmref <- clockInit
    logQ <- queueInit
    fd <- openLogFile (log_file spec)
    buf <- createBuffer 81920
    mvar <- newMVar $ FileBuffer fd buf
    let closer = Catch $ singleFileLoggerFinalizer mvar
    installHandler sigTERM closer Nothing
    installHandler sigINT  closer Nothing
    forkIO $ singleFileFlusher mvar
    forkIO $ singleFileRotator spec mvar
    return (mightyLogger (tmref,logQ), singleFileLogger logQ mvar)

singleFileLogger :: LogQ -> MVar FileBuffer -> IO ()
singleFileLogger logQ mvar = forever $ do
    bss <- dequeue logQ
    FileBuffer fd buf@(Buffer ptr _ off) <- takeMVar mvar
    mbuf' <- copyByteStrings buf bss
    case mbuf' of
        Nothing -> do
            fdWriteBuf fd ptr (fromIntegral off)
            let buf' = clearBuffer buf
            Just buf'' <- copyByteStrings buf' bss
            putMVar mvar (FileBuffer fd buf'')
        Just buf' -> putMVar mvar (FileBuffer fd buf')

singleFileLoggerFinalizer :: MVar FileBuffer -> IO ()
singleFileLoggerFinalizer mvar = do
    FileBuffer fd buf <- takeMVar mvar
    closeLogFile fd buf
    exitImmediately ExitSuccess

singleFileFlusher :: MVar FileBuffer -> IO ()
singleFileFlusher mvar = forever $ do
    FileBuffer fd buf@(Buffer ptr _ off) <- takeMVar mvar
    if off /= 0 then do
        fdWriteBuf fd ptr (fromIntegral off)
        let buf' = clearBuffer buf
        putMVar mvar (FileBuffer fd buf')
    else
        putMVar mvar (FileBuffer fd buf)
    threadDelay 5000000

singleFileRotator :: FileLogSpec -> MVar FileBuffer -> IO ()
singleFileRotator spec mvar = forever $ do
    siz <- fileSize <$> getFileStatus (log_file spec)
    when (fromIntegral siz > log_file_size spec) $ do
        FileBuffer fd buf <- takeMVar mvar
        closeLogFile fd buf
        rotate (log_file spec) (log_backup_number spec)
        fd' <- openLogFile (log_file spec)
        putMVar mvar (FileBuffer fd' buf)
    threadDelay 10000000
