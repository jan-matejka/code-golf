module Jmcgmqp.Uname (
  getKernel,
) where

import Foreign.C (peekCString, throwErrnoIfMinus1_)
import Foreign.Marshal (allocaBytes)
import Bindings.Uname (uname, release)

getKernel :: IO String
getKernel = allocaBytes 512 $ \ ptr ->
           do throwErrnoIfMinus1_ "uname" $ uname ptr
              peekCString $ release ptr

