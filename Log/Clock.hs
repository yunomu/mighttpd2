module Log.Clock (clockInit, getDate, TimeRef) where

import Control.Applicative
import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Time
import System.Locale

type TimeRef = IORef ByteString

getDate :: TimeRef -> IO ByteString
getDate = readIORef

clockInit :: IO (TimeRef)
clockInit = do
    ref <- timeByteString >>= newIORef
    forkIO $ clock ref
    return ref

clock :: TimeRef -> IO ()
clock ref = do
    tmstr <- timeByteString
    atomicModifyIORef ref (\_ -> (tmstr, undefined))
    threadDelay 1000000
    clock ref

timeByteString :: IO ByteString
timeByteString =
    BS.pack . formatTime defaultTimeLocale "%d/%b/%Y:%T %z" <$> getZonedTime

