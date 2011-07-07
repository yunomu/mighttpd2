{-# LANGUAGE DoAndIfThenElse #-}

module Log.MultiStdout where

import Control.Monad
import Log.Buffer
import Log.Control
import Log.Multi
import System.Posix
import Foreign.C.Error (eAGAIN, eINTR)
import System.Posix.IO.ByteString

multiWriter :: Fd -> Share -> IO ()
multiWriter fd share = forever $ do
    xs <- recvControl rhdl
    let len = read xs :: ByteCount
    nonblockingWrite ptr len
    sendControl whdl "done\n"
  where
    buf = share_buf share
    Buffer ptr _ _ = buf
    whdl = share_whdl share
    rhdl = share_rhdl share
    nonblockingWrite p len = do
        res <- tryFdWriteBuf fd p len
        case res of
            Left x -> if x `elem` [eAGAIN, eINTR] then
                          nonblockingWrite p len
                      else
                          return ()
            _ -> return ()

multiStdoutWriter :: Share -> IO ()
multiStdoutWriter = multiWriter stdOutput
