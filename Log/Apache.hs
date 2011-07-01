{-# LANGUAGE OverloadedStrings #-}

module Log.Apache where

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Classic
import Log.Queue
import Log.Clock

mightyLogger :: (TimeRef,LogQ) -> Request -> Status -> Maybe Integer -> IO ()
mightyLogger (ref,logQ) req st msize = do
    addr <- getPeerAddr (remoteHost req)
    tmstr <- getDate ref
    enqueue logQ [
        BS.pack addr
      , " - - ["
      , tmstr
      , "] \""
      , requestMethod req
      , " "
      , rawPathInfo req
      , "\" "
      , BS.pack (show . statusCode $ st)
      , " "
      , BS.pack (maybe "-" show msize)
      , " \"" -- size
      , lookupRequestField' "referer" req
      , "\" \""
      , lookupRequestField' "user-agent" req
      , "\"\n"
      ]
