{-# LANGUAGE ForeignFunctionInterface #-}

module Log.SharedMemory where

import Data.Bits
import Data.Int
import Data.Word
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr

#include <sys/mman.h>

foreign import ccall unsafe "mmap"
    c_mmap :: Ptr Word8 -> (#type size_t) -> CInt -> CInt -> CInt -> (#type off_t) -> IO (Ptr Word8)

getSharedMemory :: Int -> IO (Ptr Word8)
getSharedMemory siz = throwErrnoIf checkMinus1 "getSharedMemory" $
    c_mmap nullPtr
           (fromIntegral siz)
           ((#const PROT_READ) .|. (#const PROT_WRITE))
           ((#const MAP_ANON) .|. (#const MAP_SHARED))
           (-1)
           0
  where
    checkMinus1 ptr = ptrToIntPtr ptr == -1
