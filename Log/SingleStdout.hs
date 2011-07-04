module Log.SingleStdout where

import Control.Monad
import Data.ByteString as BS
import Log.Apache
import Log.Clock
import Log.Queue
import Network.Wai.Application.Classic

singleStdoutLoggerInit :: IO (Logger, IO ())
singleStdoutLoggerInit = do
    ref <- clockInit
    logQ <- queueInit
    return (mightyLogger (ref,logQ), singleStdoutLogger logQ)

singleStdoutLogger :: LogQ -> IO ()
singleStdoutLogger logQ = forever $ do
    bss <- dequeue logQ
    BS.putStr $ BS.concat bss

