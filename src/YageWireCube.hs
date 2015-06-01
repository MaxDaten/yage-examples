{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TupleSections          #-}

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
import Data.List (cycle)

import Yage.Rendering.Pipeline.Voxel.VisualizeVoxel


winSettings :: WindowConfig
winSettings = WindowConfig
  { windowSize = (1200, 800)
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

data HeadView = HeadView
    { _viewCamera     :: !Camera
    , _theHead        :: !Head
    , _lightPosRed    :: !(V3 Double)
    , _lightPosBlue   :: !(V3 Double)
    }
    deriving (Show)

data Head = Head
    { _headPosition    :: !(V3 Double)
    , _headOrientation :: !(Quaternion Double)
    , _headScale       :: !(V3 Double)
    }
    deriving (Show)

makeLenses ''HeadView
makeLenses ''Head

-------------------------------------------------------------------------------
-- * View Definition

data HeadScene = HeadScene
  { _headScene          :: DeferredScene
  , _headCamera         :: HDRCamera
  , _headRenderSettings :: DeferredSettings
  }

makeLenses ''HeadScene

-- * Main Logic

mainWire :: (HasTime Double (YageTimedInputState t), Real t, Floating t, Show t) => YageWire t () HeadScene
mainWire = proc () -> do
  hdrCam     <- hdrController . overA camera cameraControl -< defaultHDRCamera $ idCamera (deg2rad 75) 0.1 100
  deferredConf <- deferredSettingsController -< def

  skyDome    <- skyDomeW  -< hdrCam^.camera.position
  cubeEntity <- cubeW -< ()
  redLight   <- pointlightRedW -< ()

  let env = emptyEnvironment
        & sky ?~ skyDome
        -- & lights.dir   .~ singleton directionalLight
        & lights.point .~ singleton redLight
      scene = Scene (singleton cubeEntity) env (Box (-1) 1)
  returnA -< HeadScene scene hdrCam deferredConf
 where
  directionalLight = makeDirectionalLight (V3 (-1) (-1) (-1)) (V3 1 0.953 0.918) 0.6

cubeW :: YageWire t () DeferredEntity
cubeW = acquireOnce cubeEntity
 where
  cubeEntity :: YageResource DeferredEntity
  cubeEntity = Entity
    <$> (fromMesh =<< cubeMesh) <*> cubeMaterial <*> pure idTransformation
    <&> transformation.scale         .~ 1
    <&> transformation.position      .~ V3 (-0.5) (-0.5) (-0.5)

  cubeMesh :: YageResource (Mesh YGMVertex)
  cubeMesh = meshRes $ loadYGM id ("res" </> "model" </> "Cube.ygm", mkSelection ["face"])

  cubeMaterial :: YageResource (GBaseMaterial Texture2D)
  cubeMaterial = do
    albedoTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"metal"</>"iron-dungeon"</>"Door_IronDungeonDoor_1k_alb"<.>"png")
    normalTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"metal"</>"iron-dungeon"</>"Door_IronDungeonDoor_1k_n"<.>"png")
    roughTex  <- textureRes =<< (imageRes $ "res"</>"tex"</>"metal"</>"iron-dungeon"</>"Door_IronDungeonDoor_1k_r"<.>"png")
    metalTex  <- textureRes =<< (imageRes $ "res"</>"tex"</>"metal"</>"iron-dungeon"</>"Door_IronDungeonDoor_1k_h"<.>"png")
    gBaseMaterialRes defaultGBaseMaterial
      <&> albedo.materialTexture     .~ albedoTex
      <&> normalmap.materialTexture  .~ normalTex
      <&> roughness.materialTexture  .~ roughTex
      <&> metallic.materialTexture   .~ metalTex
      -- <&> roughness.materialColor    .~ 0


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
    envMap <- (textureRes =<< (cubeCrossMipsRes Strip ("res"</>"tex"</>"env"</>"Sea"</>"small"</>"strip_half.jpg")))
    radMap <- (textureRes =<< (cubeCrossMipsRes Strip ("res"</>"tex"</>"env"</>"Sea"</>"pmrem"</>"*_m<->.png")))
    --envMap <- textureRes (sameFaces $ blackDummy :: Cubemap (Image PixelRGB8))
    --radMap <- textureRes (sameFaces $ constColorPx gray :: Cubemap (Image PixelRGB8))
    return $ SkyMaterial (defaultMaterialSRGB & materialTexture .~ envMap)
                         (defaultMaterialSRGB & materialTexture .~ radMap & materialColor .~ Mat.opaque Mat.gray)

pointlightRedW :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t a Light
pointlightRedW = proc _ -> do
  pos <- arr (\t-> V3 0 0 (-0.5) + V3 (sin t * 0.5) 0 (cos t * 0.5)) . arr (/2) . time -< ()
  returnA -< makePointlight pos 5 (V3 1 0 0) 0.6

-- * Movement

camStartPos :: V3 Double
camStartPos = V3 0 0.0 2

mouseSensitivity :: V2 Double
mouseSensitivity = V2 (-pi/500) (-pi/500)

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = arcBallRotation mouseControlled . arr (0,) . fpsCameraMovement camStartPos wasdControlled

hdrController :: Num t => YageWire t HDRCamera HDRCamera
hdrController =
  exposure      <~~ ( spin (0, 10) 1.0 <<< (( 0.05 <$) <$> keyJustPressed Key'Period)  &&&
                                           ((-0.05 <$) <$> keyJustPressed Key'Comma) ) >>>

  exposureBias  <~~ ( spin (-10, 10) (-0.05) <<< (( 0.01 <$) <$> keyJustPressed Key'M)     &&&
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


deferredSettingsController :: Num t => YageWire t DeferredSettings DeferredSettings
deferredSettingsController =
  overA activeVoxelAmbientOcclusion (toggle (keyJustReleased Key'F9) False True)
  -- . overA voxelDebugModes (hold . popOnEvent (cycle modes) . keyJustReleased Key'F12)

-- * The Main

main :: IO ()
main = yageMain "yage-cube" configuration mainWire yDeferredLighting (1/60)

-- * Boilerplate

instance HasMonitorOptions Configuration where
  monitorOptions = mainMonitorOptions

instance HasWindowConfig Configuration where
  windowConfig = mainWindowConfig

instance HasApplicationConfig Configuration where
  applicationConfig = mainAppConfig

instance HasScene HeadScene DeferredEntity DeferredEnvironment where
  scene = headScene

instance HasHDRCamera HeadScene where
  hdrCamera = headCamera

instance HasEntities HeadScene (Seq DeferredEntity) where
  entities = headScene.entities

instance HasDeferredSettings HeadScene where
  deferredSettings = headRenderSettings

instance LinearInterpolatable HeadScene where
  lerp _ _ = id
