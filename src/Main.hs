----------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE MultilineStrings  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------
module Main where
----------------------------------------------------------------------
import Control.Monad (void, when, unless)
import Data.Map (fromList)
----------------------------------------------------------------------
import Miso
import Miso.Canvas qualified as Canvas
import qualified Miso.DSL as J
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.Lens
import Miso.Lens qualified as Lens
import Miso.Lens.TH (makeLenses)
import Miso.CSS qualified as CSS
----------------------------------------------------------------------
import THREE.Internal
import THREE.OrbitControls
import THREE.PerspectiveCamera
import THREE.Scene
import THREE.Stats
import THREE.WebGLRenderer
----------------------------------------------------------------------
-- Model
----------------------------------------------------------------------
data Model = Model
  { _mTime    :: Double
  , _mRunning :: Bool
  } deriving (Eq)
----------------------------------------------------------------------
makeLenses ''Model
----------------------------------------------------------------------
mkModel :: Model
mkModel = Model 0 True
----------------------------------------------------------------------
-- FFI
----------------------------------------------------------------------
appendInBody :: JSVal -> MisoString -> MisoString -> IO ()
appendInBody v left top = do
  body <- jsg "document" ! "body"
  _ <- body # "appendChild" $ [v]
  vStyle <- v ! "style"
  setField vStyle "left" left
  setField vStyle "top" top
----------------------------------------------------------------------
-- Three
----------------------------------------------------------------------
data Context = Context
  { ctxRenderer :: WebGLRenderer
  , ctxScene    :: Scene
  , ctxCamera   :: PerspectiveCamera
  , ctxControls :: OrbitControls
  , ctxClock    :: JSVal
  , ctxStats    :: Stats
  }
----------------------------------------------------------------------
instance ToJSVal Context where
  toJSVal Context {..} = do
    o <- create
    setField o "renderer" =<< toJSVal ctxRenderer
    setField o "scene"    =<< toJSVal ctxScene
    setField o "camera"   =<< toJSVal ctxCamera
    setField o "controls" =<< toJSVal ctxControls
    setField o "clock"    ctxClock
    setField o "stats"    =<< toJSVal ctxStats
    toJSVal o
----------------------------------------------------------------------
instance FromJSVal Context where
  fromJSVal o = do
    renderer' <- fromJSVal =<< o ! "renderer"
    scene'    <- fromJSVal =<< o ! "scene"
    camera'   <- fromJSVal =<< o ! "camera"
    controls' <- fromJSVal =<< o ! "controls"
    clock'    <- o ! "clock"
    stats'    <- fromJSVal =<< o ! "stats"
    pure $ Context
      <$> renderer'
      <*> scene'
      <*> camera'
      <*> controls'
      <*> pure clock'
      <*> stats'
----------------------------------------------------------------------
initCanvas :: DOMRef -> Three Context
initCanvas domRef = do
  w_ <- fromJSValUnchecked =<< jsg "window" ! "innerWidth"  :: IO Double
  h  <- fromJSValUnchecked =<< jsg "window" ! "innerHeight" :: IO Double

  rendOpts <- create
  setField rendOpts "canvas" domRef
  setField rendOpts "antialias" True
  rendCls <- jsg ("THREE" :: MisoString) ! ("WebGLRenderer" :: MisoString)
  rendRaw <- J.new rendCls rendOpts
  let renderer = WebGLRenderer rendRaw
  renderer & setSize (round w_, round h, True)
  rendVal <- toJSVal renderer
  pixRatio <- fromJSValUnchecked =<< jsg "window" ! "devicePixelRatio" :: IO Double
  void $ rendVal # "setPixelRatio" $ [pixRatio]
  acesVal <- jsg "THREE" ! "ACESFilmicToneMapping"
  setField rendVal "toneMapping" acesVal
  setField rendVal "toneMappingExposure" (1.0 :: Double)

  scene <- THREE.Scene.new
  sceneVal <- toJSVal scene

  skyCls <- jsg "Sky"
  skyVal <- J.new skyCls ()
  skyScale <- skyVal ! "scale"
  void $ skyScale # "setScalar" $ [10000.0 :: Double]
  void $ sceneVal # "add" $ [skyVal]
  skyMat <- skyVal ! "material"
  uniforms <- skyMat ! "uniforms"
  turbidity <- uniforms ! "turbidity"
  setField turbidity "value" (0.0 :: Double)
  rayleigh <- uniforms ! "rayleigh"
  setField rayleigh "value" (3.0 :: Double)
  mieG <- uniforms ! "mieDirectionalG"
  setField mieG "value" (0.7 :: Double)
  sunPosUniform <- uniforms ! "sunPosition"
  sunPosVec <- sunPosUniform ! "value"
  void $ sunPosVec # "set" $ ((-0.8) :: Double, 0.19 :: Double, 0.56 :: Double)

  pmremCls <- jsg "THREE" ! "PMREMGenerator"
  pmremVal <- J.new pmremCls [rendVal]
  envResult <- pmremVal # "fromScene" $ [skyVal]
  envTex <- envResult ! "texture"
  setField sceneVal "environment" envTex
  void $ pmremVal # "dispose" $ ()

  camera <- THREE.PerspectiveCamera.new (40, w_ / h, 1, 100)
  camVal <- toJSVal camera
  camPos <- camVal ! "position"
  void $ camPos # "set" $ (5.0 :: Double, 2.0 :: Double, 8.0 :: Double)

  controls <- THREE.OrbitControls.new (camera, domRef)
  ctrlVal <- toJSVal controls
  setField ctrlVal "enableDamping" True
  ctrlTarget <- ctrlVal ! "target"
  void $ ctrlTarget # "set" $ (0.0 :: Double, 0.7 :: Double, 0.0 :: Double)
  void $ THREE.OrbitControls.update () controls

  clockCls <- jsg "THREE" ! "Clock"
  clockVal <- J.new clockCls ()

  winVal <- jsg "window"
  setField winVal "__tokyoMixer__" jsNull

  dracoLoaderCls <- jsg "DRACOLoader"
  dracoLoaderVal <- J.new dracoLoaderCls ()
  void $ dracoLoaderVal # "setDecoderPath" $
    ["https://www.gstatic.com/draco/versioned/decoders/1.5.6/" :: MisoString]

  gltfLoaderCls <- jsg "GLTFLoader"
  gltfLoaderVal <- J.new gltfLoaderCls ()
  void $ gltfLoaderVal # "setDRACOLoader" $ [dracoLoaderVal]

  onLoad_ <- asyncCallback1 $ \gltfVal -> do
    modelVal <- gltfVal ! "scene"
    modelPos <- modelVal ! "position"
    void $ modelPos # "set" $ (1.0 :: Double, 1.0 :: Double, 0.0 :: Double)
    modelScale <- modelVal ! "scale"
    void $ modelScale # "set" $ (0.01 :: Double, 0.01 :: Double, 0.01 :: Double)
    void $ sceneVal # "add" $ [modelVal]
    mixerCls <- jsg "THREE" ! "AnimationMixer"
    mixerVal <- J.new mixerCls [modelVal]
    anims <- gltfVal ! "animations"
    anim0 <- anims Miso.!! (0 :: Int)
    action <- mixerVal # "clipAction" $ [anim0]
    void $ action # "play" $ ()
    w2 <- jsg "window"
    setField w2 "__tokyoMixer__" mixerVal

  void $ gltfLoaderVal # "load" $
    ( "https://threejs.org/examples/models/gltf/LittlestTokyo.glb" :: MisoString
    , onLoad_
    )

  stats <- THREE.Stats.new ()
  statsDOM <- stats THREE.Internal.^. dom
  appendInBody statsDOM "0px" "0px"

  pure (Context renderer scene camera controls clockVal stats)
----------------------------------------------------------------------
drawCanvas :: Model -> Context -> Three ()
drawCanvas model Context {..} =
  when (model Lens.^. mRunning) $ do
    delta :: Double <- fromJSValUnchecked =<< do ctxClock # "getDelta" $ ()
    mixerVal <- jsg "window" ! "__tokyoMixer__"
    nullMixer <- isNull mixerVal
    unless nullMixer $
      void $ mixerVal # "update" $ [delta]
    void $ THREE.OrbitControls.update () ctxControls
    ctxRenderer & render (ctxScene, ctxCamera)
    THREE.Stats.update () ctxStats
----------------------------------------------------------------------
-- App
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
          , CSS.zIndex 100
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
          , CSS.zIndex 100
          ]
      ]
      [ button_
          [ Styles (fromList [CSS.width "80px"])
          , onClick ActionSwitchRunning
          ]
          [ if model Miso.Lens.^. mRunning then "pause" else "run" ]
      ]
  ]
----------------------------------------------------------------------
handleUpdate :: Action -> Effect parent props Model Action
handleUpdate = \case
  Tick t               -> mTime Miso.Lens..= t
  ActionSwitchRunning  -> mRunning Miso.Lens.%= not
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
            [ "three"         =: "https://cdn.jsdelivr.net/npm/three@v0.178.0/build/three.module.js"
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
----------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------
