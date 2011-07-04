module Log.File where

import Control.Monad
import System.Directory
import System.Posix
import Log.Buffer

rotate :: FilePath -> Int -> IO ()
rotate path n = mapM_ move srcdsts
  where
    dsts' = reverse . ("":) . map (('.':). show) $ [0..n-1]
    dsts = map (path++) dsts'
    srcs = tail dsts
    srcdsts = zip srcs dsts
    move (src,dst) = do
        exist <- doesFileExist src
        when exist $ renameFile src dst

openLogFile :: FilePath -> IO Fd
openLogFile file =
    openFd file WriteOnly (Just 0o644) defaultFileFlags { append = True }

closeLogFile :: Fd -> Buffer -> IO ()
closeLogFile fd (Buffer ptr _ off) = do
    fdWriteBuf fd ptr (fromIntegral off)
    closeFd fd
