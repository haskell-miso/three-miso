----------------------------------------------------------------------
{-# LANGUAGE MultilineStrings  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------
module Main where
----------------------------------------------------------------------
import Data.Map (fromList)
----------------------------------------------------------------------
import Miso
import Miso.Canvas qualified as Canvas
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.Lens
import Miso.CSS qualified as CSS
----------------------------------------------------------------------
import Model
import MyThree
import FFI
----------------------------------------------------------------------
data Action
  = ActionTime Double
  | ActionSwitchRunning
----------------------------------------------------------------------
handleView :: Model -> View Model Action
handleView model = div_ [] 
  [ h1_
    [ CSS.style_ [ CSS.fontFamily "monospace" ] ]
    [ "🍜 🧊 three-miso" ]
  , div_
    []
    (map mkCanvas [1..5])
  , p_
    []
    [ button_ 
      [ Styles (fromList [CSS.width "100px"])
      , onClick ActionSwitchRunning
      ]
      [ pauseOrRun ]
    ]
  , p_ []
    [ a_ [ href_ "https://github.com/haskell-miso/three-miso" ] [ "source" ]
    , " - "
    , a_ [ href_ "https://haskell-miso.github.io/three-miso/" ] [ "demo" ]
    ]
  , p_ []
    [ "Use left click + drag to rotate, and middle mouse scroll too zom in each scene"
    ]
  ]
  where
    pauseOrRun = if model ^. mRunning then "pause" else "run"  
    mkCanvas offset = Canvas.canvas_
      [ width_ (ms canvasWidth)
      , height_ (ms canvasHeight)
      , Styles (fromList [CSS.margin "5px"])
      ] 
      initCanvas
      (drawCanvas model offset)
----------------------------------------------------------------------
handleUpdate :: Action -> Transition Model Action
handleUpdate (ActionTime t) = do
  mTime .= t
  io (ActionTime <$> myGetTime)
handleUpdate ActionSwitchRunning = do
  mRunning %= not
----------------------------------------------------------------------
myGetTime :: JSM Double
myGetTime = (* 0.001) <$> now
----------------------------------------------------------------------
main :: IO ()
main = do
  -- display FPS
  stats1 <- THREE.Stats.new ()
  stats1Dom <- stats1 ^. dom
  appendInBody stats1Dom "230px" "15px"

  -- display ms
  stats2 <- THREE.Stats.new ()
  stats2 & showPanel 1
  stats2Dom <- stats2 ^. dom
  appendInBody stats2Dom "330px" "15px"

  run $ startApp
    (component mkModel handleUpdate handleView)
      { logLevel = DebugAll
      , initialAction = Just (ActionTime 0)
#ifndef WASM
      , scripts =
        [ ImportMap 
            [ "three" =: "https://cdn.jsdelivr.net/npm/three@v0.178.0/build/three.module.js"
            , "three/addons/" =: "https://cdn.jsdelivr.net/npm/three@v0.178.0/examples/jsm/"
            ]
        , Module
            """
            import * as THREE from 'three';
            window.THREE = THREE;
            import { OrbitControls } from 'three/addons/controls/OrbitControls.js';
            window.OrbitControls = OrbitControls;
            import Stats from 'three/addons/libs/stats.module.js';
            window.Stats = Stats;
            """
         ]
#endif
      }
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------
