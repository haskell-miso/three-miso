----------------------------------------------------------------------
module FFI 
  ( appendInBody
  ) where
----------------------------------------------------------------------
import Control.Monad (void)
import Control.Lens hiding ((#))
import Language.Javascript.JSaddle as J
----------------------------------------------------------------------
appendInBody :: JSVal -> JSString -> JSString -> JSM ()
appendInBody v left top = do
  void $ jsg "document" ^. js "body" ^. js1 "appendChild" v
  void $ v ^. js "style" ^. jss "left" left
  void $ v ^. js "style" ^. jss "top" top
----------------------------------------------------------------------
