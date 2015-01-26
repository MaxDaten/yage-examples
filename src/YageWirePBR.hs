{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where


import Yage
import Yage.Lens hiding ((<.>))
import Yage.Math hiding (normal)
import Yage.Wire hiding ((<>))
import Yage.Wire.Utils

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
import Data.Traversable (sequenceA)


winSettings :: WindowConfig
winSettings = WindowConfig
    { windowSize = (1600, 1000)
    , windowHints =
        [ WindowHint'ContextVersionMajor  4
        , WindowHint'ContextVersionMinor  1
        , WindowHint'OpenGLProfile        OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat  True
        , WindowHint'RefreshRate          60
        -- , WindowHint'OpenGLDebugContext   True
        -- , WindowHint'Resizable            False
        -- , WindowHint'Decorated            False
        ]
     }


data Configuration = Configuration
  { _mainAppConfig      :: ApplicationConfig
  , _mainWindowConfig   :: WindowConfig
  , _mainMonitorOptions :: MonitorOptions
  }

makeLenses ''Configuration

appConf :: ApplicationConfig
appConf = defaultAppConfig{ logPriority = INFO }

configuration :: Configuration
configuration = Configuration appConf winSettings (MonitorOptions "localhost" 8080 True False)

-------------------------------------------------------------------------------
-- View Definition

data PBRScene = PBRScene
  { _pbrScene          :: DeferredScene
  , _pbrCamera         :: HDRCamera
  }

makeLenses ''PBRScene


pbrTestScene :: (HasTime Double (YageTimedInputState t), Real t, Floating t, Show t) => YageWire t () PBRScene
pbrTestScene = proc () -> do
  hdrCam  <- hdrCameraW -< ()
  skyDome <- skyDomeW  -< hdrCam^.camera.position
  floorEntity <- groundEntityW -< ()

  returnA -< PBRScene
    { _pbrScene          = Scene (fromList [floorEntity]) (emptyEnvironment & sky ?~ skyDome & lights .~ fromList [spotLight01])
    , _pbrCamera         = hdrCam
    }
 where
  directionalLight = makeDirectionalLight (V3 (0) (-1) (-1)) (V3 1 0.953 0.918) 0.75
  -- spotLight01 = makeSpotlight ( V3 0 8 8 ) ( V3 0 (-5) (-5) ) 50 60 ( V3 1 0 0 ) 10
  spotLight01 = makeSpotlight ( V3 8 8 0 ) ( V3 (-5) (-5) 0 ) 50 60 ( V3 1 0 0 ) 10

hdrCameraW :: Real t => YageWire t () HDRCamera
hdrCameraW =
  let initCamera = idCamera (deg2rad 75) 0.1 100
  in hdrController . (defaultHDRCamera <$> cameraControl . pure initCamera)

hdrController :: Num t => YageWire t HDRCamera HDRCamera
hdrController =
  exposure      <~~ ( spin (0, 10) 1.0 <<< (( 0.05 <$) <$> keyJustPressed Key'Period)  &&&
                                          ((-0.05 <$) <$> keyJustPressed Key'Comma) ) >>>

  exposureBias  <~~ ( spin (-10, 10) 0.0 <<< (( 0.01 <$) <$> keyJustPressed Key'M)     &&&
                                            ((-0.01 <$) <$> keyJustPressed Key'N) ) >>>
  whitePoint    <~~ pure 11.2       >>>
  bloomSettings <~~ pure bloomConfig
 where
  bloomConfig :: HDRBloomSettings
  bloomConfig = defaultBloomSettings
    & bloomFactor           .~ 0.7
    & bloomPreDownsampling  .~ 1
    & bloomGaussPasses      .~ 5
    & bloomWidth            .~ 1
    & bloomThreshold        .~ 0.5


skyDomeW :: Real t => YageWire t (V3 Double) DeferredSky
skyDomeW = proc pos -> do
  skye <- acquireOnce skyEntity -< ()
  returnA -< skye
    & transformation.position  .~ pos
    & transformation.scale     .~ 50

 where
  environmentTexture = "res"</>"tex"</>"misc"</>"blueprint"</>"Seamless Blueprint Textures"</>"1"<.>"png"
  skyEntity :: YageResource DeferredSky
  skyEntity = Entity <$> fromMesh skydome <*> skyMaterial <*> pure idTransformation
  skyMaterial :: YageResource (SkyMaterial Texture)
  skyMaterial = do
    envMap <- textureRes =<< (sameFaces <$> (imageRes environmentTexture ))
    radMap <- textureRes (sameFaces $ blackDummy :: Cubemap (Image PixelRGB8))
    return $ SkyMaterial (defaultMaterialSRGB & materialTexture .~ envMap & materialColor .~ darken 0.1 (opaque white))
                         (defaultMaterialSRGB & materialTexture .~ radMap)

groundEntityW :: YageWire t () DeferredEntity
groundEntityW =
 acquireOnce (Entity <$> fromMesh planeMesh <*> groundMaterial <*> pure (idTransformation & scale._xz .~ 13 & position._y .~ 0.75) )
 where
 groundMaterial :: YageResource (GBaseMaterial Texture)
 groundMaterial = do
    albedoTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"floor_d"<.>"png")
    normalTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"floor_n"<.>"png")
    gBaseMaterialRes defaultGBaseMaterial
      <&> albedo.materialTexture     .~ albedoTex
      <&> albedo.stpFactor           .~ 2.0
      <&> normalmap.materialTexture  .~ normalTex
      <&> normalmap.stpFactor        .~ 2.0
      <&> roughness.materialColor    .~ 0.5

-- * Controller Wires

camStartPos :: V3 Double
camStartPos = V3 0 1 5

mouseSensitivity :: V2 Double
mouseSensitivity = V2 (pi/500) (pi/500)

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = arcBallRotation mouseControlled . arr (0,) . fpsCameraMovement camStartPos wasdControlled

-- | Boilerplate

main :: IO ()
main = yageMain "yage-pbr" configuration pbrTestScene yDeferredLighting (1/60)

instance HasMonitorOptions Configuration where
  monitorOptions = mainMonitorOptions

instance HasWindowConfig Configuration where
  windowConfig = mainWindowConfig

instance HasApplicationConfig Configuration where
  applicationConfig = mainAppConfig

instance HasScene PBRScene DeferredEntity DeferredEnvironment where
  scene = pbrScene

instance HasHDRCamera PBRScene where
  hdrCamera = pbrCamera

instance HasEntities PBRScene (Seq DeferredEntity) where
  entities = pbrScene.entities

instance LinearInterpolatable PBRScene where
  lerp _ _ = id

