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
----------------------------------------------------------------------
data Action
  = Tick Double
  | ActionSwitchRunning
----------------------------------------------------------------------
handleView :: props -> Model -> View Model Action
handleView _ model = div_
  [ CSS.style_ [ CSS.backgroundColor (CSS.hex "e2e0e0"), CSS.margin "0", CSS.padding "0", CSS.overflow "hidden" ] ]
  [ div_
      [ CSS.style_
          [ CSS.position "absolute"
          , CSS.top "10px"
          , CSS.left "10px"
          , CSS.zIndex "100"
          , CSS.color (CSS.hex "000")
          , CSS.fontFamily "monospace"
          , CSS.fontSize "13px"
          ]
      ]
      [ a_ [ href_ "https://github.com/haskell-miso/three-miso" ] [ "littlest-tokyo-miso" ]
      , " webgl - animation - keyframes"
      , br_ []
      , "Model: "
      , a_ [ href_ "https://artstation.com/artwork/1AGwX" ] [ "Littlest Tokyo" ]
      , " by "
      , a_ [ href_ "https://artstation.com/glenatron" ] [ "Glen Fox" ]
      , ", CC Attribution."
      ]
  , Canvas.canvas_
      [ CSS.style_
          [ CSS.display "block"
          , CSS.position "fixed"
          , CSS.top "0"
          , CSS.left "0"
          ]
      ]
      initCanvas
      (drawCanvas model)
  , div_
      [ CSS.style_
          [ CSS.position "absolute"
          , CSS.bottom "10px"
          , CSS.left "10px"
          , CSS.zIndex "100"
          ]
      ]
      [ button_
          [ Styles (fromList [CSS.width "80px"])
          , onClick ActionSwitchRunning
          ]
          [ if model ^. mRunning then "pause" else "run" ]
      ]
  ]
----------------------------------------------------------------------
handleUpdate :: Action -> Effect parent props Model Action
handleUpdate = \case
  Tick t          -> mTime .= t
  ActionSwitchRunning -> mRunning %= not
----------------------------------------------------------------------
main :: IO ()
main =
  startApp defaultEvents
    (component mkModel handleUpdate handleView)
      { logLevel = DebugAll
      , subs = [ rAFSub Tick ]
#ifndef WASM
      , scripts =
        [ ImportMap
            [ "three"        =: "https://cdn.jsdelivr.net/npm/three@v0.178.0/build/three.module.js"
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
            import { Sky } from 'three/addons/objects/Sky.js';
            window.Sky = Sky;
            import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';
            window.GLTFLoader = GLTFLoader;
            import { DRACOLoader } from 'three/addons/loaders/DRACOLoader.js';
            window.DRACOLoader = DRACOLoader;
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
