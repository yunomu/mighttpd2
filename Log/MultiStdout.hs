module Log.MultiStdout where

import Control.Monad
import Log.Buffer
import Log.Control
import Log.Multi
import System.Posix

multiWriter :: Fd -> Share -> IO ()
multiWriter fd share = forever $ do
    xs <- recvControl rhdl
    let len = read xs :: ByteCount
    fdWriteBuf fd ptr len
    sendControl whdl "done\n"
  where
    buf = share_buf share
    Buffer ptr _ _ = buf
    whdl = share_whdl share
    rhdl = share_rhdl share

multiStdoutWriter :: Share -> IO ()
multiStdoutWriter = multiWriter stdOutput
