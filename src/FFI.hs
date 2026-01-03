----------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
module FFI 
  ( appendInBody
  ) where
----------------------------------------------------------------------
import Miso
import Control.Monad (void)
----------------------------------------------------------------------
appendInBody :: JSVal -> MisoString -> MisoString -> IO ()
appendInBody v left top = do
  body <- jsg "document" ! "body"
  body # "appendChild" $ [v]
  vStyle <- v ! "style"
  setField vStyle "left" left
  setField vStyle "top" top
----------------------------------------------------------------------
