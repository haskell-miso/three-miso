----------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
----------------------------------------------------------------------
module MyThree where
----------------------------------------------------------------------
import Control.Monad (void, when, unless)
import Data.Function ((&))
----------------------------------------------------------------------
import Miso
import qualified Miso.DSL as J
import Miso.Lens qualified as Lens
----------------------------------------------------------------------
import THREE.Internal
import THREE.OrbitControls
import THREE.PerspectiveCamera
import THREE.Scene
import THREE.Stats
import THREE.WebGLRenderer
----------------------------------------------------------------------
import Model
import FFI
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
  -- Window dimensions for a fullscreen renderer
  w_ <- fromJSValUnchecked =<< jsg "window" ! "innerWidth"  :: IO Double
  h <- fromJSValUnchecked =<< jsg "window" ! "innerHeight" :: IO Double

  -- Renderer
  renderer <- THREE.WebGLRenderer.new (Just domRef)
  renderer & setSize (round w_, round h, True)
  rendVal <- toJSVal renderer
  pixRatio <- fromJSValUnchecked =<< jsg "window" ! "devicePixelRatio" :: IO Double
  void $ rendVal # "setPixelRatio" $ [pixRatio]
  acesVal <- jsg "THREE" ! "ACESFilmicToneMapping"
  setField rendVal "toneMapping" acesVal

  -- Scene
  scene <- THREE.Scene.new
  sceneVal <- toJSVal scene

  -- Sky
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

  -- PMREMGenerator for image-based lighting from the Sky
  pmremCls <- jsg "THREE" ! "PMREMGenerator"
  pmremVal <- J.new pmremCls [rendVal]
  envResult <- pmremVal # "fromScene" $ [skyVal]
  envTex <- envResult ! "texture"
  setField sceneVal "environment" envTex
  void $ pmremVal # "dispose" $ ()

  -- Camera
  camera <- THREE.PerspectiveCamera.new (40, w_ / h, 1, 100)
  camVal <- toJSVal camera
  camPos <- camVal ! "position"
  void $ camPos # "set" $ (5.0 :: Double, 2.0 :: Double, 8.0 :: Double)

  -- OrbitControls with damping and look-at target
  controls <- THREE.OrbitControls.new (camera, domRef)
  ctrlVal <- toJSVal controls
  setField ctrlVal "enableDamping" True
  ctrlTarget <- ctrlVal ! "target"
  void $ ctrlTarget # "set" $ (0.0 :: Double, 0.7 :: Double, 0.0 :: Double)
  void $ THREE.OrbitControls.update () controls

  -- THREE.Clock for per-frame delta time
  clockCls <- jsg "THREE" ! "Clock"
  clockVal <- J.new clockCls ()

  -- Store AnimationMixer in a JS global so the async callback can write to it
  winVal <- jsg "window"
  setField winVal "__tokyoMixer__" jsNull

  -- DRACOLoader (compressed geometry decoder)
  dracoLoaderCls <- jsg "DRACOLoader"
  dracoLoaderVal <- J.new dracoLoaderCls ()
  void $ dracoLoaderVal # "setDecoderPath" $
    ["https://www.gstatic.com/draco/versioned/decoders/1.5.6/" :: MisoString]

  -- GLTFLoader
  gltfLoaderCls <- jsg "GLTFLoader"
  gltfLoaderVal <- J.new gltfLoaderCls ()
  void $ gltfLoaderVal # "setDRACOLoader" $ [dracoLoaderVal]

  -- Async callback: runs once the .glb model is ready
  onLoad <- asyncCallback1 $ \gltfVal -> do
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
    , onLoad
    )

  -- Stats overlay
  stats <- THREE.Stats.new ()
  statsDOM <- stats ^. dom
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
