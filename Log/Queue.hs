module Log.Queue where

import Control.Applicative
import Control.Concurrent
import Log.Msg

newtype LogQ = LogQ (Chan Msg)

enqueue :: LogQ -> Msg -> IO ()
enqueue (LogQ chan) msg = writeChan chan msg

dequeue :: LogQ -> IO Msg
dequeue (LogQ chan) = readChan chan

queueInit :: IO (LogQ)
queueInit = LogQ <$> newChan
