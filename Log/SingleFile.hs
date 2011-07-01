{-# LANGUAGE DoAndIfThenElse #-}

module Log.SingleFile where

import Data.IORef
import Log.Buffer
import Log.File
import Log.Queue

-- undefined -> LogSpec

singleFileLogger :: LogQ -> Buffer -> IORef Buffer -> IO ()
singleFileLogger logQ buf ref = do
    bss <- dequeue logQ
    mbuf' <- copyByteStrings buf bss
    case mbuf' of
        Nothing -> do
            closeLogFile undefined buf
            rotate undefined undefined
            buf' <- openLogFile undefined undefined
            Just buf'' <- copyByteStrings buf' bss
            atomicModifyIORef ref (\_ -> (buf'',undefined))
            singleFileLogger logQ buf'' ref
        Just buf' -> do
            atomicModifyIORef ref (\_ -> (buf',undefined))
            singleFileLogger logQ buf' ref

singleFileLoggerFinalizer :: IORef Buffer -> IO ()
singleFileLoggerFinalizer ref = do
    buf <- readIORef ref
    closeLogFile undefined buf
