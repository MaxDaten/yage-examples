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
  headEntity <- headW -< ()
  redLight   <- pointlightRedW -< ()

  let env = emptyEnvironment
        & sky ?~ skyDome
        & lights.dir   .~ singleton directionalLight
        & lights.point .~ singleton redLight
      scene = Scene (singleton headEntity) env (Box (-10) 10)
  returnA -< HeadScene scene hdrCam deferredConf
 where
  directionalLight = makeDirectionalLight (V3 (-1) (-1) (-1)) (V3 1 0.953 0.918) 0.6

headW :: YageWire t () DeferredEntity
headW = acquireOnce headEntity
 where
  headEntity :: YageResource DeferredEntity
  headEntity = Entity
    <$> (fromMesh =<< headMesh) <*> headMaterial <*> pure idTransformation
    <&> transformation.scale         .~ 4
    <&> transformation.position      .~ V3 0 0.5 0

  headMesh :: YageResource (Mesh YGMVertex)
  headMesh = meshRes $ loadYGM id ("res" </> "model" </> "head.ygm", mkSelection [])

  headMaterial :: YageResource (GBaseMaterial Texture2D)
  headMaterial = do
    albedoTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"head"</>"big"</>"head_albedo.jpg")
    normalTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"head"</>"big"</>"head_tangent.jpg")
    gBaseMaterialRes defaultGBaseMaterial
      <&> albedo.materialTexture     .~ albedoTex
      <&> albedo.materialTransformation.scale._y .~ (-1)
      <&> normalmap.materialTexture  .~ normalTex
      <&> normalmap.materialTransformation.scale._y .~ (-1)
      <&> roughness.materialColor    .~ 0.8

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
{-
pLightBlueW :: Real t => YageWire t a Light
pLightBlueW =
    Light <$> (Pointlight <$> arr (\t-> V3 0 1 (1) + V3 0.5 0.5 0.5 * V3 (cos t) (sin t) (sin t)) . arr (/2) . time
                          <*> pure 1)
          <*> (pure $ V3 0.85 0.85 1.0)
          <*> (pure 0.1)
-}


-- * Movement

camStartPos :: V3 Double
camStartPos = V3 0 0.1 1

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


headRotationByInput :: (Real t) => YageWire t a (Quaternion Double)
headRotationByInput =
    let acc         = 20
        att         = 0.87
    in
    smoothRotationByKey acc att ( yAxis ) Key'Right
  . smoothRotationByKey acc att (-yAxis ) Key'Left
  . smoothRotationByKey acc att ( xAxis ) Key'Up
  . smoothRotationByKey acc att (-xAxis ) Key'Down
  . 1

deferredSettingsController :: Num t => YageWire t DeferredSettings DeferredSettings
deferredSettingsController = overA activeVoxelAmbientOcclusion (toggle (keyJustReleased Key'F12) True False)

-- * The Main

main :: IO ()
main = yageMain "yage-head" configuration mainWire yDeferredLighting (1/60)

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

