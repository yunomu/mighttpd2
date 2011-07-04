module Log.Clock (clockInit, getDate, TimeRef) where

import Control.Applicative
import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Time
import System.Locale

newtype TimeRef = TimeRef (IORef ByteString)

getDate :: TimeRef -> IO ByteString
getDate (TimeRef ref) = readIORef ref

clockInit :: IO (TimeRef)
clockInit = do
    ref <- timeByteString >>= newIORef
    let timeref = TimeRef ref
    forkIO $ clock timeref
    return timeref

clock :: TimeRef -> IO ()
clock timeref@(TimeRef ref) = do
    tmstr <- timeByteString
    atomicModifyIORef ref (\_ -> (tmstr, undefined))
    threadDelay 1000000
    clock timeref

timeByteString :: IO ByteString
timeByteString =
    BS.pack . formatTime defaultTimeLocale "%d/%b/%Y:%T %z" <$> getZonedTime

