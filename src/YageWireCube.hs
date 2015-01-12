{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import Data.Traversable (sequenceA)

import Yage
import Yage.Lens hiding ((<.>))
import Yage.Math hiding (normal)
import Yage.Wire hiding ((<>))

import Yage.Camera
import Yage.Scene
import qualified Yage.Vertex as V
import Yage.HDR
import Yage.UI.GUI hiding (Texture)
import Yage.Transformation
import qualified Yage.Resources as Res
import Yage.Material as Mat
import Yage.Rendering.Resources.GL
import Yage.Rendering.Pipeline.Deferred
import Yage.Rendering.Pipeline.Deferred.SkyPass
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
type CubeEntity = Entity (RenderData Word32 YGMVertex) (GBaseMaterial Texture)
data CubeScene = CubeScene
  { _cubeScene          :: DeferredScene
  , _cubeMainViewport   :: Viewport Int
  , _cubeCamera         :: HDRCamera
  , _cubeSceneRenderer  :: RenderSystem CubeScene ()
  }

makeLenses ''CubeScene

mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () CubeScene
mainWire = proc () -> do
  pipeline <- acquireOnce yDeferredLighting -< ()
  cam      <- overA camera cameraControl -< initCamera
  scene    <- sceneWire -< cam^.camera
  returnA -< CubeScene scene (defaultViewport 1200 800) cam pipeline


sceneWire :: Real t => YageWire t Camera DeferredScene
sceneWire = proc cam -> do
  cubeEntity <- cubeEntityW >>> (transformation.orientation <~~ cubeRotationByInput) -< ()
  skyDome    <- skyDomeW -< cam^.position
  returnA -< Scene
    { _sceneEntities    = fromList [ cubeEntity ]
    , _sceneEnvironment = emptyEnvironment & sky    ?~ skyDome
                                           & lights .~ fromList [ mainLight, spotlight, directionLight ]
    }
 where
  mainLight = Light
    { _lightType           = Pointlight
    , _lightTransformation = idTransformation & scale .~ 5 & position .~ (V3 0 5 0)
    , _lightColor          = 1
    , _lightIntensity      = 50
    }
  spotlight = makeSpotlight (V3 0 5 0) (V3 0 (-1) 0) 10 13 (V3 1 1 1) 50
  directionLight = makeDirectionalLight (V3 0 (-1) (-1)) 1 25

cubeEntityW :: YageWire t b CubeEntity
cubeEntityW = acquireOnce (cube <&> transformation.scale //~ 2)
 where
  cube :: YageResource CubeEntity
  cube = Entity <$> (fromMesh =<< cubeMesh) <*> cubeMaterial <*> pure idTransformation
  cubeMaterial :: YageResource (GBaseMaterial Texture)
  cubeMaterial = do
    albedoTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"floor_d"<.>"png")
    normalTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"floor_n"<.>"png")
    gBaseMaterialRes defaultGBaseMaterial
      <&> albedo.materialTexture     .~ albedoTex
      <&> normalmap.stpFactor        .~ 2.0
      <&> normalmap.materialTexture  .~ normalTex
      <&> normalmap.stpFactor        .~ 2.0
      <&> roughness.materialColor    .~ 0.5
  cubeMesh :: YageResource (Mesh YGMVertex)
  cubeMesh = meshRes $ loadYGM id ( "res" </> "model" </> "Cube.ygm", mkSelection ["face"] )

skyDomeW :: YageWire t (V3 Double) DeferredSky
skyDomeW = proc pos -> do
  s <- acquireOnce skyEntity -< ()
  returnA -< s & transformation.scale     .~ 50
               & transformation.position  .~ pos
 where
  skyEntity :: YageResource DeferredSky
  skyEntity = Entity <$> fromMesh skydome <*> skyMaterial <*> pure idTransformation
  skyMaterial :: YageResource (SkyMaterial Texture)
  skyMaterial = do
    envMap <- textureRes =<< (sameFaces <$> (imageRes $ "res"</>"tex"</>"misc"</>"blueprint"</>"Seamless Blueprint Textures"</>"1"<.>"png" ))
    radMap <- textureRes (sameFaces $ blackDummy :: Cubemap (Image PixelRGB8))
    return $ SkyMaterial (defaultMaterialSRGB & materialTexture .~ envMap & materialColor .~ darken 0.1 (opaque white))
                         (defaultMaterialSRGB & materialTexture .~ radMap)

initCamera = defaultHDRCamera ( idCamera (deg2rad 75) 0.1 10000 )
  & exposure           .~ 2
  & exposureBias       .~ 0.0
  & whitePoint         .~ 11.2
  & bloomSettings      .~ (defaultBloomSettings
    & bloomFactor           .~ 0.7
    & bloomPreDownsampling  .~ 2
    & bloomGaussPasses      .~ 5
    & bloomWidth            .~ 2
    & bloomThreshold        .~ 0.5)


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



--}
camStartPos :: V3 Double
camStartPos = V3 0 0 2

mouseSensitivity :: V2 Double
mouseSensitivity = V2 (pi/500) (pi/500)

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = arcBallRotation mouseControlled . arr (0,) . fpsCameraMovement camStartPos wasdControlled

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

instance HasScene CubeScene DeferredEntity DeferredEnvironment where
  scene = cubeScene

instance HasHDRCamera CubeScene where
  hdrCamera = cubeCamera

instance HasEntities CubeScene (Seq CubeEntity) where
  entities = cubeScene.entities

instance LinearInterpolatable CubeScene where
  lerp _ _ = id

