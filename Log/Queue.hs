module Log.Queue where

import Control.Concurrent
import Log.Msg

type LogQ = Chan Msg

enqueue :: LogQ -> Msg -> IO ()
enqueue = undefined

dequeue :: LogQ -> IO Msg
dequeue = undefined
