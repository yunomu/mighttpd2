module Log.Multi where

import Control.Concurrent
import Control.Monad
import Foreign.Ptr
import Log.Apache
import Log.Buffer
import Log.Clock
import Log.Control
import Log.Queue
import Log.SharedMemory
import Network.Wai.Application.Classic
import System.IO

data Share = Share {
    share_buf :: !Buffer 
  , share_rhdl :: !Handle 
  , share_whdl :: !Handle
  }

prepareShare :: Int -> IO ([Share],[Share])
prepareShare n = do
    let len = 81920
        starts = take n [0,len..]
    ptr <- getSharedMemory (len * n)
    let ptrs = map (ptr `plusPtr`) starts
        bufs = map (toBuffer len) ptrs
    chans <- replicateM n makeControlChannel
    let toShare buf ((h1,h2),(h3,h4)) = (Share buf h1 h2,Share buf h3 h4)
        res = unzip $ zipWith toShare bufs chans
    return res

multiLoggerInit :: Share -> IO (Logger, IO ())
multiLoggerInit share = do
    tmref <- clockInit
    logQ <- queueInit
    mvar <- newMVar share
{-    
    let closer = Catch $ multiLoggerFinalizer mvar
    installHandler sigTERM closer Nothing
    installHandler sigINT  closer Nothing
-}
    forkIO $ multiFlusher mvar
    return (mightyLogger (tmref,logQ), multiLogger logQ mvar)

multiLogger :: LogQ -> MVar Share -> IO ()
multiLogger logQ mvar = forever $ do
    bss <- dequeue logQ
    share <- takeMVar mvar
    mbuf' <- copyByteStrings (share_buf share) bss
    case mbuf' of
        Nothing -> do
            buf' <- flush share
            Just buf'' <- copyByteStrings buf' bss
            putMVar mvar share { share_buf = buf'' }
        Just buf' -> putMVar mvar share { share_buf = buf' }

{-
multiLoggerFinalizer :: MVar Share -> IO ()
multiLoggerFinalizer mvar = do
    Share fd buf <- takeMVar mvar
    closeLog fd buf
    exitImmediately ExitSuccess
-}

multiFlusher :: MVar Share -> IO ()
multiFlusher mvar = forever $ do
    threadDelay 5000000
    share <- takeMVar mvar
    buf <- flush share
    putMVar mvar share { share_buf = buf }

flush :: Share -> IO Buffer
flush share =
    if isEmpty buf then
        return buf
    else do
        sendControl whdl $ show len ++ "\n"
        recvControl rhdl
        return $ clearBuffer buf
  where
    buf = share_buf share
    whdl = share_whdl share
    rhdl = share_rhdl share
    len = usedLength buf

