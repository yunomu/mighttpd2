module Queue where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Sequence hiding (reverse, zip)

type Element = [ByteString]
type QueueRef = IORef (Seq Element)

emptyQ :: IO QueueRef
emptyQ = newIORef empty

enqueue :: Element -> QueueRef -> IO ()
enqueue elm qref = atomicModifyIORef qref $ \q -> (elm <| q, ())

dequeue :: QueueRef -> IO (Maybe Element)
dequeue qref = atomicModifyIORef qref $ \q ->
    case viewr q of
        EmptyR    -> (q,  Nothing)
        q' :> elm -> (q', Just elm)
