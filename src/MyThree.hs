----------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
----------------------------------------------------------------------
module MyThree where
----------------------------------------------------------------------
import Control.Monad (void, when)
import Data.Function ((&))
import Data.Foldable (traverse_)
import GHC.Generics
import Language.Javascript.JSaddle as JS
----------------------------------------------------------------------
import Miso
import Miso.Lens qualified as Lens
----------------------------------------------------------------------
import THREE.BoxGeometry
import THREE.Controls
import THREE.Internal
import THREE.Light
import THREE.Mesh
import THREE.MeshLambertMaterial
import THREE.Object3D
import THREE.OrbitControls
import THREE.PerspectiveCamera
import THREE.PointLight
import THREE.Scene
import THREE.Stats
import THREE.SphereGeometry
import THREE.TextureLoader
import THREE.Vector3
import THREE.WebGLRenderer
----------------------------------------------------------------------
import Model
import FFI
----------------------------------------------------------------------
canvasWidth, canvasHeight :: Int
canvasWidth = 200
canvasHeight = 150
----------------------------------------------------------------------
canvasWidthD, canvasHeightD :: Double
canvasWidthD = fromIntegral canvasWidth
canvasHeightD = fromIntegral canvasHeight
----------------------------------------------------------------------
data Context = Context
  { renderer  :: THREE.WebGLRenderer.WebGLRenderer
  , scene     :: THREE.Scene.Scene
  , camera    :: THREE.PerspectiveCamera.PerspectiveCamera
  , cube      :: THREE.Mesh.Mesh
  , stats     :: [THREE.Stats.Stats]
  } deriving (Generic, FromJSVal, ToJSVal)
----------------------------------------------------------------------
initCanvas :: DOMRef -> Three Context
initCanvas domref = do
  scene1 <- THREE.Scene.new

  light1 <- THREE.PointLight.new ()
  light1 & intensity .= 300
  light1 ^. position !.. setXYZ 8 8 8
  void $ scene1 & add light1

  material1 <- THREE.MeshLambertMaterial.new
  geometry1 <- THREE.SphereGeometry.new
  mesh1 <- THREE.Mesh.new (geometry1, material1)
  mesh1 & position !. x .= (-1)

  texture2 <- THREE.TextureLoader.new >>= load "miso.png"
  material2 <- THREE.MeshLambertMaterial.new
  material2 & THREE.MeshLambertMaterial.map .= Just texture2
  geometry2 <- THREE.BoxGeometry.new (1, 1, 1)
  mesh2 <- THREE.Mesh.new (geometry2, material2)
  (mesh2 ^. position) !.. setXYZ 1 0 0 

  traverse_ (`add` scene1) [mesh1, mesh2]

  camera1 <- THREE.PerspectiveCamera.new (70, canvasWidthD/canvasHeightD, 0.1, 100)
  camera1 & position !. z .= 3

  renderer1 <- THREE.WebGLRenderer.new (Just domref)
  renderer1 & setSize (canvasWidth, canvasHeight, True)

  controls1 <- THREE.OrbitControls.new (camera1, domref)
  controls1 & enabled .= True

  -- display FPS
  stats1 <- THREE.Stats.new ()
  stats1Dom <- stats1 ^. dom
  appendInBody stats1Dom "230px" "15px"

  -- display ms
  stats2 <- THREE.Stats.new ()
  stats2 & showPanel 1
  stats2Dom <- stats2 ^. dom
  appendInBody stats2Dom "330px" "15px"

  pure (Context renderer1 scene1 camera1 mesh2 [stats1, stats2])
----------------------------------------------------------------------
drawCanvas :: Model -> Double -> Context -> Three ()
drawCanvas model speed Context {..} = 
  when (model Lens.^. mRunning) $ do
    cube & rotation !. y .= (speed * model Lens.^. mTime)
    renderer & render (scene, camera)
    traverse_ (THREE.Stats.update ()) stats
----------------------------------------------------------------------
