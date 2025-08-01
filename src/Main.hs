{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import Data.Map (fromList)
import Language.Javascript.JSaddle (JSM)

import Miso
import Miso.Canvas qualified as Canvas
import Miso.Lens
import Miso.String (ms)
import Miso.Style qualified as Style

import Model
import MyThree

----------------------------------------------------------------------
-- actions
----------------------------------------------------------------------

data Action
  = ActionTime Double
  | ActionSwitchRunning

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Model -> View Action
handleView model = div_ [] 
  [ h1_ [] [ "three-miso" ]
  , div_ [] (map mkCanvas [1..5])
  , p_ [] [ button_ 
              [ Styles (fromList [Style.width "100px"])
              , onClick ActionSwitchRunning
              ]
              [ pauseOrRun ] ]
  , p_ []
      [ a_ [ href_ "https://github.com/haskell-miso/three-miso" ] [ "source" ]
      , " - "
      , a_ [ href_ "https://haskell-miso.github.io/three-miso/" ] [ "demo" ]
      ]
  , p_ [] [ "Use left click + drag to rotate, and middle mouse scroll too zom in each scene" ]
  ]
  where
    pauseOrRun = if model ^. mRunning then "pause" else "run"
    
    mkCanvas offset = Canvas.canvas_
      [ width_ (ms canvasWidth)
      , height_ (ms canvasHeight)
      , Styles (fromList [Style.margin "5px"])
      ] 
      initCanvas
      (drawCanvas model offset)

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate (ActionTime t) = do
  mTime .= t
  io (ActionTime <$> myGetTime)

handleUpdate ActionSwitchRunning = do
  mRunning %= not

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------


myGetTime :: JSM Double
myGetTime = (* 0.001) <$> now

main :: IO ()
main = run $ do
  let model = mkModel
  startComponent
    (component model handleUpdate handleView)
      { logLevel = DebugAll
      , initialAction = Just (ActionTime 0)
      }

----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
