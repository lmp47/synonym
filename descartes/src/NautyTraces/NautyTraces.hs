{-# LINE 1 "NautyTraces.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-------------------------------------------------------------------------------
-- Module    :  NautyTraces.NautyTraces
-- Copyright :  (c) 2017 Lauren Pick
-- Interfaces to nauty + Traces for finding automorphisms of digraphs
-------------------------------------------------------------------------------
module NautyTraces.NautyTraces where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign

-- header file for C API


example :: IO()
example = do
  let nv = 3
  let nde = 2
  v <- makeCSizeArray [0,0,1]
  d <- makeCIntArray [0,1,1]
  e <- makeCIntArray [0,0]
  lab <- makeCIntArray [1,2,0]
  ptn <- makeCIntArray [1,0,0]
  perm <- useNauty nv nde v d e lab ptn >>= peek
  putStrLn (show $ perms perm)
  res <- freePerms
  return ()

makeCIntArray :: [Int] -> IO (Ptr CInt)
makeCIntArray arr = newArray (map fromIntegral arr)

makeCSizeArray :: [Int] -> IO (Ptr CSize)
makeCSizeArray arr = newArray (map fromIntegral arr)

-- The data structure for storing automorphisms
data Perms = Perms { perms :: [[Int]]
                   , count :: Int }

instance Storable Perms where
  alignment _ = 8
{-# LINE 44 "NautyTraces.hsc" #-}
  sizeOf    _ = (24)
{-# LINE 45 "NautyTraces.hsc" #-}
  peek ptr    = do
                ccperms <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 47 "NautyTraces.hsc" #-}
                ccount <- (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 48 "NautyTraces.hsc" #-}
                cnv <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 49 "NautyTraces.hsc" #-}
                let count = fromIntegral (ccount :: CInt)
                cperms <- peekArray count (ccperms :: Ptr (Ptr CInt))
                perms <- mapM (peekArray (fromIntegral (cnv :: CInt))) cperms
                return (Perms (map (map fromIntegral) perms) count)
  poke _ _   = return ()

foreign import ccall unsafe "use_nauty"
  useNauty :: CInt -> CSize -> Ptr CSize -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr Perms)

foreign import ccall unsafe "free_perms"
  freePerms :: IO ()

--foreign import ccall safe "use_traces"
--  useTraces :: CInt -> CSize -> Ptr CSize -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO (Perms)
