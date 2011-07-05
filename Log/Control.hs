module Log.Control where

import System.IO
import System.Posix

makeControlChannel :: IO ((Handle,Handle),(Handle,Handle))
makeControlChannel = do
    (r1,w1) <- makeControlChannel'
    (r2,w2) <- makeControlChannel'
    return ((r1,w2),(r2,w1))

makeControlChannel' :: IO (Handle,Handle)
makeControlChannel' = do
    (rfd,wfd) <- createPipe
    rhdl <- fdToHandle rfd
    whdl <- fdToHandle wfd
    hSetBuffering rhdl LineBuffering
    hSetBuffering whdl LineBuffering
    return (rhdl,whdl)

sendControl :: Handle -> String -> IO ()
sendControl = hPutStr

recvControl :: Handle -> IO String
recvControl = hGetLine
