{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yage hiding ((><), point)
import Yage.Lens hiding ((<.>))
import Yage.Math hiding (normal, (><), point)
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
import qualified Data.Sequence as Seq
import Data.Sequence ((><))

-------------------------------------------------------------------------------
-- * Scene Definition


data SponzaScene = SponzaScene
  { _sponzaScene          :: DeferredScene
  , _sponzaCamera         :: HDRCamera
  , _sponzaRenderSettings :: DeferredSettings
  }

makeLenses ''SponzaScene

mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () SponzaScene
mainWire = proc () -> do
    world   <- worldEntityW -< ()
    hdrCam  <- hdrCameraW -< ()
    skyDome <- skyDomeW -< hdrCam^.camera.position
    deferreConfig <- deferredSettingsController -< def

    let env = emptyEnvironment & sky ?~ skyDome & lights.point .~ singleton directionalLight
    returnA -< SponzaScene
      { _sponzaScene          = Scene (singleton world) env (Box (-4) 4)
      , _sponzaCamera         = hdrCam
      , _sponzaRenderSettings = deferreConfig & ambientOcclusionMinDiameterFactor .~ 5.0
      }
 where
  directionalLight = makeDirectionalLight (V3 (-1) (-1) (-1)) (V3 1 0.953 0.918) 0.0
  baseLight pos = makePointlight pos 10 (V3 1.0 1.0 1.0) 0.1
  genLights mkLight = map mkLight
    [ V3 (5 * x - 2.5) (5 * y) (5 * z - 2.5)
    | x <- [0..2]
    , y <- [0..2]
    , z <- [0..2]
    ]

-- * World

worldEntityW :: YageWire t () DeferredEntity
worldEntityW = acquireOnce sponzaEntity <&> transformation.scale //~ 400
 where
  sponzaEntity :: YageResource DeferredEntity
  sponzaEntity = Entity
    <$> (fromMesh =<< sponzaMesh) <*> sponzaMaterial <*> pure idTransformation

  sponzaMaterial :: YageResource (GBaseMaterial Texture2D)
  sponzaMaterial = do
    albedoTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"default"<.>"png")
    normalTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"floor_n"<.>"png")
    gBaseMaterialRes defaultGBaseMaterial
      -- <&> albedo.materialTexture     .~ albedoTex
      -- <&> normalmap.materialTexture  .~ normalTex
      -- <&> albedo.stpFactor           .~ 2.0
      -- <&> normalmap.stpFactor        .~ 2.0
      <&> roughness.materialColor    .~ 0.5

  sponzaMesh :: YageResource (Mesh YGMVertex)
  sponzaMesh = meshRes $ loadYGM id ( "res" </> "model" </> "env" </> "sponza.ygm", mkSelection [] )

-- * Sky Dome

skyDomeW :: Real t => YageWire t (V3 Double) DeferredSky
skyDomeW = proc pos -> do
  skye <- acquireOnce skyEntity -< ()
  returnA -< skye
    & transformation.position  .~ pos
    & transformation.scale     .~ 50

 where
  skyEntity :: YageResource DeferredSky
  skyEntity = Entity <$> fromMesh skydome <*> skyMaterial <*> pure idTransformation
  skyMaterial :: YageResource (SkyMaterial TextureCube)
  skyMaterial = do
    -- (imageRes $ texDir</>"misc"</>"blueprint"</>"Seamless Blueprint Textures"</>"1"<.>"png")
    envMap <- (textureRes =<< (cubeCrossMipsRes Strip ("res"</>"tex"</>"env"</>"Sea"</>"small"</>"strip_half.jpg")))
    radMap <- (textureRes =<< (cubeCrossMipsRes Strip ("res"</>"tex"</>"env"</>"Sea"</>"pmrem"</>"*_m<->.png")))
    -- radMap <- textureRes (sameFaces $ blackDummy :: Cubemap (Image PixelRGB8))
    -- envMap <- textureRes (sameFaces $ blackDummy :: Cubemap (Image PixelRGB8))
    return $ SkyMaterial (defaultMaterialSRGB & materialTexture .~ envMap)
                         (defaultMaterialSRGB & materialTexture .~ radMap)


-- * Camera Control

hdrCameraW :: Real t => YageWire t () HDRCamera
hdrCameraW =
  let initCamera = idCamera (deg2rad 75) 0.1 250
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
    & bloomPreDownsampling  .~ 4
    & bloomGaussPasses      .~ 7
    & bloomWidth            .~ 1
    & bloomThreshold        .~ 0.5


-- _cameraPosition = V3 (-3.02029386283718) 0.3159171871883328 (-6.059381486659588e-2),
-- _cameraOrientation = Quaternion 0.6918433043791026 (V3 5.0077448525642006e-2 (-0.7184294686934339) 5.2001825427923414e-2),
camStartPos :: V3 Double
camStartPos = V3 (-3) 0.3 (0.05)

mouseSensitivity :: V2 Double
mouseSensitivity = V2 (pi/500) (pi/500)

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = fpsCameraMovement camStartPos wasdControlled . fpsCameraRotation mouseControlled

deferredSettingsController :: Num t => YageWire t DeferredSettings DeferredSettings
deferredSettingsController =
  overA showDebugOverlay (toggle (keyJustReleased Key'F12) False True)
  . overA activeVoxelAmbientOcclusion (toggle (keyJustReleased Key'F9) False True)

-- * Boilerplate


winSettings :: WindowConfig
winSettings = WindowConfig
    { windowSize = (1600, 1000)
    , windowHints =
        [ WindowHint'ContextVersionMajor  4
        , WindowHint'ContextVersionMinor  4
        , WindowHint'OpenGLProfile        OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat  True
        , WindowHint'RefreshRate          60
        , WindowHint'sRGBCapable          True
        , WindowHint'OpenGLDebugContext   True
        -- , WindowHint'Resizable            False
        , WindowHint'Decorated            True
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

main :: IO ()
main = yageMain "yage-sponza" configuration mainWire yDeferredLighting (1/60)

instance HasMonitorOptions Configuration where
  monitorOptions = mainMonitorOptions

instance HasWindowConfig Configuration where
  windowConfig = mainWindowConfig

instance HasApplicationConfig Configuration where
  applicationConfig = mainAppConfig

instance HasScene SponzaScene DeferredEntity DeferredEnvironment where
  scene = sponzaScene

instance HasHDRCamera SponzaScene where
  hdrCamera = sponzaCamera

instance HasEntities SponzaScene (Seq DeferredEntity) where
  entities = sponzaScene.entities

instance HasDeferredSettings SponzaScene where
  deferredSettings = sponzaRenderSettings

instance LinearInterpolatable SponzaScene where
  lerp _ _ = id

