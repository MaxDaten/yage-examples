{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Traversable (sequenceA)

import Yage
import Yage.Lens hiding ((<.>))
import Yage.Math hiding (normal)
import Yage.Wire hiding ((<>))

import Yage.Camera
import Yage.Scene
import Yage.HDR
import Yage.Texture
import Yage.UI.GUI hiding (Texture)
import Yage.Transformation
import qualified Yage.Resources as Res
import Yage.Material as Mat
import Yage.Rendering.Resources.GL
import Yage.Rendering.Pipeline.Deferred
import Yage.Formats.Ygm
import Yage.Examples.Shared

winSettings :: WindowConfig
winSettings = WindowConfig
  { windowSize = (1200, 800)
  , windowHints =
    [ WindowHint'ContextVersionMajor  4
    , WindowHint'ContextVersionMinor  1
    , WindowHint'OpenGLProfile        OpenGLProfile'Core
    , WindowHint'OpenGLForwardCompat  True
    , WindowHint'RefreshRate          60
    --, WindowHint'Resizable            False
    --, WindowHint'Decorated            False
    ]
  }

data Configuration = Configuration
  { _mainAppConfig      :: ApplicationConfig
  , _mainWindowConfig   :: WindowConfig
  , _mainMonitorOptions :: MonitorOptions
  }

makeLenses ''Configuration

appConf :: ApplicationConfig
appConf = defaultAppConfig{ logPriority = WARNING }

configuration :: Configuration
configuration = Configuration appConf winSettings (MonitorOptions "localhost" 8080 True False)

-- type SceneEnvironment = Environment () ()
type CubeEntity = Entity (RenderData (SVector Word32) (SVector YGMVertex)) (GBaseMaterial Texture)
data CubeScene = CubeScene
  { _cubeScene          :: DeferredScene CubeEntity () ()
  , _cubeMainViewport   :: Viewport Int
  , _cubeSceneRenderer  :: RenderSystem CubeScene ()
  }

makeLenses ''CubeScene

mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () CubeScene
mainWire = proc () -> do
  pipeline <- acquireOnce yDeferredLighting -< ()
  scene    <- sceneWire -< ()
  returnA -< CubeScene scene (defaultViewport 1200 800) pipeline


sceneWire :: Real t => YageWire t () (DeferredScene CubeEntity () ())
sceneWire = proc () -> do
  cam    <- overA hdrCameraHandle cameraControl -< initCamera
  cubeEntity <- cubeEntityW >>> (transformation.orientation <~~ cubeRotationByInput) -< ()
  returnA -< Scene
    { _sceneEntities    = fromList [ cubeEntity ]
    , _sceneEnvironment = ()
    , _sceneCamera      =  cam
    , _sceneGui         = ()
    }

cubeEntityW :: YageWire t b CubeEntity
cubeEntityW = acquireOnce (cube <&> transformation.scale //~ 2)
 where
  cube :: YageResource CubeEntity
  cube = Entity <$> (fromMesh =<< cubeMesh) <*> cubeMaterial <*> pure idTransformation
  cubeMaterial :: YageResource (GBaseMaterial Texture)
  cubeMaterial = do
    albedoTex <- texture2DRes =<< (imageRes $ "res"</>"tex"</>"floor_d"<.>"png")
    normalTex <- texture2DRes =<< (imageRes $ "res"</>"tex"</>"floor_n"<.>"png")
    gBaseMaterialRes defaultGBaseMaterial
      <&> albedo.materialTexture  .~ albedoTex
      <&> normal.stpFactor        .~ 2.0
      <&> normal.materialTexture  .~ normalTex
      <&> normal.stpFactor        .~ 2.0
  cubeMesh :: YageResource (Mesh YGMVertex)
  cubeMesh = meshRes $ loadYGM id ( "res" </> "model" </> "Cube.ygm", mkSelection ["face"] )

bloomSettings = defaultBloomSettings
  & bloomFactor           .~ 0.7
  & bloomPreDownsampling  .~ 2
  & bloomGaussPasses      .~ 5
  & bloomWidth            .~ 2
  & bloomThreshold        .~ 0.5

initCamera = defaultHDRCamera ( idCamera (deg2rad 75) 0.1 10000 )
  & hdrExposure           .~ 2
  & hdrExposureBias       .~ 0.0
  & hdrWhitePoint         .~ 11.2
  & hdrBloomSettings      .~ bloomSettings

{--


mainWire = proc _ -> do

    sky    <- skyDomeW -< cam^.hdrCameraHandle.cameraLocation

    returnA -< emptyScene cam emptyGUI
                & sceneSky      ?~ sky
                & sceneEntities .~ fromList [ boxEntity ]
                & sceneLights   .~ fromList [ frontLight ]

    where
    texDir  = "res"</>"tex"

    frontLight  = Light
                    { _lightType      = Pointlight (V3 0 1 25) 100
                    , _lightColor     = 1
                    , _lightIntensity = 50
                    }

    skyDomeW :: YageWire t (V3 Double) SkyEntity
    skyDomeW = proc pos -> do
        tex <- cubeTextureToTexture "SkyCube" . pure <$> constTextureW skyTex -< ()
        returnA -< skydome & materials.skyEnvironmentMap
                                      .Mat.matTexture .~ tex
                           & entityPosition           .~ pos
                           & entityScale              .~ 50

    skyTex  = mkTexture2D "SkyBlueprint" <$> (imageRes $ texDir</>"misc"</>"blueprint"</>"Seamless Blueprint Textures"</>"1"<.>"png")


--}
camStartPos :: V3 Double
camStartPos = V3 0 0 2

mouseSensitivity :: V2 Double
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = fpsCameraMovement camStartPos wasdControlled . fpsCameraRotation mouseControlled

cubeRotationByInput :: (Real t) => YageWire t a (Quaternion Double)
cubeRotationByInput =
    let acc         = 20
        att         = 0.87
    in
   smoothRotationByKey acc att ( yAxis ) Key'Right
 . smoothRotationByKey acc att (-yAxis ) Key'Left
 . smoothRotationByKey acc att ( xAxis ) Key'Up
 . smoothRotationByKey acc att (-xAxis ) Key'Down
 . 1

main :: IO ()
main = yageMain "yage-cube" configuration mainWire (1/60)

instance HasMonitorOptions Configuration where
  monitorOptions = mainMonitorOptions

instance HasWindowConfig Configuration where
  windowConfig = mainWindowConfig

instance HasApplicationConfig Configuration where
  applicationConfig = mainAppConfig

instance HasViewport CubeScene Int where
  viewport = cubeMainViewport

instance HasRenderSystem CubeScene (ResourceT IO) CubeScene () where
  renderSystem = cubeSceneRenderer

instance HasDeferredScene CubeScene CubeEntity () () where
  deferredScene = cubeScene

instance LinearInterpolatable CubeScene where
  lerp _ _ = id

