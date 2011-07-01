module Log.File where

import Control.Applicative
import Control.Monad
import System.Directory
import System.Posix
import System.IO.MMap
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

openLogFile :: FilePath -> Int -> IO Buffer
openLogFile file siz = do
    exist <- doesFileExist file
    off <- if exist then
               fileSize <$> getFileStatus file
           else do
               createFile file 0o644 >>= closeFd
               return 0
    (ptr,len,_,_) <- mmapFilePtr file ReadWriteEx (Just (0,siz))
    return $ Buffer ptr len (fromIntegral off)

closeLogFile :: FilePath -> Buffer -> IO ()
closeLogFile file (Buffer ptr siz off) = do
    munmapFilePtr ptr siz
    setFileSize file (fromIntegral off)
