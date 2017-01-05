
{-# LANGUAGE CApiFFI #-}

{-| Haskell API to posix "truncate" function.
-}


module Trunc where

import Foreign.C            (CString, CInt(..)
                            , withCString, throwErrnoPathIfMinus1_)
import System.Posix.Types   (COff(..))

-- | returns 0 on success, -1 on failure, w/ an error code.
-- the COff is size in bytes
foreign import capi unsafe "unistd.h truncate" 
  c_truncate :: CString -> COff -> IO CInt

-- | "truncate filename size" will 'truncate' the file
-- to that size (in bytes), as per the posix function.
truncate :: FilePath -> Int -> IO ()
truncate filename size = 
  withCString filename $ \cstr -> 
    throwErrnoPathIfMinus1_ "truncate" filename $ 
      c_truncate cstr (fromIntegral size)

