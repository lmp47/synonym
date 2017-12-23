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

getSymm :: Int -> Int -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> IO (Perms)
getSymm nv nde v d e lab ptn = do
{-
  let nv = 6
  let nde = 5
  v <- makeCSizeArray [0,0,0,1,3,4]
  d <- makeCIntArray [0,0,1,2,1,1]
  e <- makeCIntArray [3,4,5,0,1]
  lab <- makeCIntArray [0,1,2,3,4,5]
  ptn <- makeCIntArray [1,1,1,1,1,0]
  perm <- useNauty nv nde v d e lab ptn >>= peek
  putStrLn (show $ perms perm)
-}
{-
  let nv = 11
  let nde = 6
  v <- makeCSizeArray [0,0,0,0,1,3,4,5,0,0,0]
  d <- makeCIntArray [0,0,0,1,2,1,1,1,0,0,0]
  e <- makeCIntArray [4,5,6,0,1,8]
  lab <- makeCIntArray [0,1,2,3,4,5,6,7,8,9,10]
  ptn <- makeCIntArray [1,1,1,1,1,1,1,1,1,1,0]
  perm <- useNauty nv nde v d e lab ptn >>= peek
  putStrLn (show $ perms perm)
-}
{-
  let nv = 15
  let nde = 15
  v <- makeCSizeArray [0,0,0,0,1,3,4,5,6,8,9,10,11,13,14]
  d <- makeCIntArray [0,0,0,1,2,1,1,1,2,1,1,1,2,1,1]
  e <- makeCIntArray [4,5,6,0,1,8,9,10,0,2,12,13,14,1,2]
  lab <- makeCIntArray [14,13,9,10,6,5,0,1,2,12,8,4,11,7,3]
  ptn <- makeCIntArray [1,1,1,1,1,1,1,1,1,1,1,1,1,1,0]
  perm <- useNauty nv nde v d e lab ptn >>= peek
  putStrLn (show $ perms perm)
  -}
  v' <- makeCSizeArray v
  d' <- makeCIntArray d
  e' <- makeCIntArray e
  lab' <- makeCIntArray lab
  ptn' <- makeCIntArray ptn
  perm <- useNauty (fromIntegral nv) (fromIntegral nde) v' d' e' lab' ptn' >>= peek
  freePerms
  return perm

-- The data structure for storing automorphisms
data Perms = Perms { perms :: [[Int]]
                   , count :: Int }

instance Storable Perms where
  alignment _ = 8
{-# LINE 88 "NautyTraces.hsc" #-}
  sizeOf    _ = (24)
{-# LINE 89 "NautyTraces.hsc" #-}
  peek ptr    = do
                ccperms <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 91 "NautyTraces.hsc" #-}
                ccount <- (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 92 "NautyTraces.hsc" #-}
                cnv <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 93 "NautyTraces.hsc" #-}
                let count = fromIntegral (ccount :: CInt)
                cperms <- peekArray count (ccperms :: Ptr (Ptr CInt))
                perms <- mapM (peekArray (fromIntegral (cnv :: CInt))) cperms
                return (Perms (map (map fromIntegral) perms) count)
  poke _ _   = return ()

foreign import ccall safe "use_nauty"
  useNauty :: CInt -> CSize -> Ptr CSize -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr Perms)

foreign import ccall safe "free_perms"
  freePerms :: IO ()

--foreign import ccall safe "use_traces"
--  useTraces :: CInt -> CSize -> Ptr CSize -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO (Perms)
