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

-------------------------------------------------------------------------------
-- * View Definition

data PBRScene = PBRScene
  { _pbrScene          :: DeferredScene
  , _pbrCamera         :: HDRCamera
  , _pbrRenderSettings :: DeferredSettings
  }

makeLenses ''PBRScene


pbrTestScene :: (HasTime Double (YageTimedInputState t), RealFrac t, Show t) => YageWire t () PBRScene
pbrTestScene = proc () -> do
  t <- time -< ()

  hdrCam  <- hdrCameraW -< ()
  deferreConfig <- deferredSettingsController -< def

  skyDome <- skyDomeW  -< hdrCam^.camera.position
  showWallMode <- toggle (keyJustReleased Key'F1) True False -< ()

  planeE  <- groundEntityW <&> transformation.scale .~ 20 -< ()
  let backOrientation = axisAngle (V3 1 0 0) (deg2rad $ 90)
      wallBack = planeE & transformation.orientation .~ backOrientation
                        & transformation.position._z .~ (-9.9)
                        & transformation.scale._y *~ (-1.0)

      leftOrientation =  axisAngle (V3 0 1 0) (deg2rad $ 90) * backOrientation
      wallLeft = planeE & transformation.orientation        .~ leftOrientation
                        & transformation.position._x        .~ -9.9
                        & materials.normalmap.stpFactor._y  *~ (-1.0)
                        & materials.albedo.stpFactor._y     *~ (-1.0)
                        & materials.albedo.materialColor    .~ Mat.opaque Mat.red

      rightOrientation = axisAngle (V3 0 1 0) (deg2rad $ -90) * backOrientation
      wallRight = planeE & transformation.orientation .~ rightOrientation
                         & transformation.position._x .~ 9.9
                         & materials.normalmap.stpFactor._y *~ (-1.0)
                         & materials.albedo.stpFactor._y *~ (-1.0)
                         & materials.albedo.materialColor    .~ Mat.opaque Mat.green
      floorE = planeE & transformation.position._y .~ -9.9
      ceilingE = planeE & transformation.orientation .~ axisAngle (V3 1 0 0) (deg2rad $ 180)
                        & transformation.position._y .~ 9.9
                        & transformation.scale._y *~ (-1.0)
                        & transformation.scale._z *~ (-1.0)
                        & materials.normalmap.stpFactor._y *~ (-1.0)
                        & materials.albedo.stpFactor._y *~ (-1.0)
  spheres     <- spheresW -< ()

  let env = emptyEnvironment
        & sky ?~ skyDome
        & lights.dir   .~ fromList [directionalLight]
        & lights.point .~ empty |> pLight0 |> pLight1 |> pLight2 |> pLight3
        & lights.spot  .~ singleton spotLight01
      entities = if showWallMode
                 then empty |> wallBack |> wallLeft |> wallRight |> floorE -- |> ceilingE
                 else empty |> floorE
  returnA -< PBRScene
    { _pbrScene          = Scene (entities >< spheres) env (Box (-20) (20))
    , _pbrCamera         = hdrCam
    , _pbrRenderSettings = deferreConfig
    }
 where
  directionalLight = makeDirectionalLight (V3 (-1) (-1) (-1)) (V3 1 0.953 0.918) 0.0
  pLight0 = makePointlight (V3 4 1 4) 4 (V3 0.3 1 0.3) 0.6
  pLight1 = pLight0 & position._z  %~ negate & lightColor .~ V3 0.3 0.3 1
  pLight2 = pLight0 & position._x  %~ negate & lightColor .~ V3 0.3 1 1
  pLight3 = pLight0 & position._xz.mapped %~ negate & lightColor .~ V3 1 0.3 1
  spotLight01 = makeSpotlight ( V3 8 8 0 ) ( V3 (-5) (-5) 0 ) 50 60 ( V3 1 0 0 ) 1

hdrCameraW :: Real t => YageWire t () HDRCamera
hdrCameraW =
  let initCamera = idCamera (deg2rad 75) 0.1 500
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
    -- radMap <- textureRes (sameFaces $ blackDummy :: Cubemap (Image PixelRGB8))
    -- envMap <- textureRes (sameFaces $ blackDummy :: Cubemap (Image PixelRGB8))
    return $ SkyMaterial (defaultMaterialSRGB & materialTexture .~ envMap)
                         (defaultMaterialSRGB & materialTexture .~ radMap)

groundEntityW :: YageWire t () DeferredEntity
groundEntityW =
 acquireOnce (Entity <$> fromMesh planeMesh <*> groundMaterial <*> pure (idTransformation & scale._xz .~ 13 & position._y .~ 0.1) )
 where
 groundMaterial :: YageResource (GBaseMaterial Texture2D)
 groundMaterial = do
    albedoTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"floor_d"<.>"png")
    normalTex <- textureRes =<< (imageRes $ "res"</>"tex"</>"floor_n"<.>"png")
    gBaseMaterialRes defaultGBaseMaterial
      <&> albedo.materialTexture     .~ albedoTex
      <&> albedo.stpFactor           .~ 2.0
      <&> normalmap.materialTexture  .~ normalTex
      <&> normalmap.stpFactor        .~ 2.0
      <&> roughness.materialColor    .~ 0.5

spheresW :: (HasTime Double (YageTimedInputState t), RealFrac t) => YageWire t () (Seq DeferredEntity)
spheresW   = proc () -> do
  s <- acquireOnce sphereEntity -< ()
  t <- time -< ()
  returnA -< empty
    |> ( s & transformation.position._xyz      .~ V3 (-4.5) (-4) (-4.5)
           & transformation.position._y        +~ sin t * 1.2
           & transformation.scale              .~ 4.5
           & materials.albedo.materialColor    .~ Mat.opaque Mat.gold
           & materials.roughness.materialColor .~ 0.3
       )
    |> ( s & transformation.position._xyz .~ V3 2.5 (-7.2) (5.0)
           & transformation.scale              .~ 2
           & materials.albedo.materialColor    .~ Mat.opaque Mat.silver
           & materials.roughness.materialColor .~ 0.2
       )

sphereEntity :: YageResource DeferredEntity
sphereEntity =
  Entity <$> (fromMesh =<< sphereMesh) <*> sphereMaterialId <*> pure idTransformation
         <&> transformation.position._y  .~ 5

spheresOnGridW :: (HasTime Double (YageTimedInputState t), RealFrac t) => YageWire t () (Seq DeferredEntity)
spheresOnGridW = proc () -> do
  t <- time -< ()
  ss <- acquireOnce (placeEntityOnGridXZ (1, 1) (V2 10 1) <$> sphereEntity) -< ()
  returnA -< ss & mapped.transformation.position._xyz .~ V3 (-2.5) (-5.0) (-5.0)
 where
  animate :: Double -> DeferredEntity -> DeferredEntity
  animate t e = e & transformation.position._x +~ sin (t*0.3) * 10.0 & transformation.position._y .~ (5.0) & transformation.position._z .~ (1.0)


sphereMaterialId :: YageResource (GBaseMaterial Texture2D)
sphereMaterialId = do
  roughTex    <- textureRes =<< (imageRes $ "res" </> "tex" </> "noise_r.png")
  normalTex   <- textureRes =<< (imageRes $ "res" </> "tex" </> "noise_t.png")
  gBaseMaterialRes defaultGBaseMaterial
    <&> roughness.materialTexture  .~ roughTex
    <&> roughness.materialColor    .~ 0.5
    <&> roughness.stpFactor        .~ 2.0
    <&> normalmap.materialTexture  .~ normalTex
    <&> normalmap.materialColor    .~ rgb 2 2 2 `withOpacity` 0.5
    <&> normalmap.stpFactor        .~ 2.0
    <&> metallic.materialColor     .~ 1.0

placeEntityOnGridXZ :: (Int, Int) -> V2 Double -> DeferredEntity -> Seq DeferredEntity
placeEntityOnGridXZ (xCnt, yCnt) (V2 dimX dimY) template = foldr (\xyp s -> s |> generate xyp) Seq.empty (gridIdx `zip` positions)
 where
  generate ((xi, yi), pos) =
    let roughValue  = xi / fromIntegral (xCnt-1)
    in template & transformation.position._xz .~ pos
                & materials.roughness.materialColor .~ (realToFrac roughValue)

  positions   = map calculatePosition gridIdx
  gridIdx     = [ (fromIntegral xi, fromIntegral yi) | xi <- [0 .. xCnt-1], yi <- [0 .. yCnt-1] ]
  startPoint  = (step + V2 (-dimX) (dimY)) ^* 0.5
  step        = 0 & _x .~  dimX / fromIntegral xCnt
                  & _y .~ -dimY / fromIntegral yCnt
  calculatePosition (xi, yi) =  startPoint + V2 xi yi * step

-- * Controller Wires

camStartPos :: V3 Double
camStartPos = V3 0 0 24

mouseSensitivity :: V2 Double
mouseSensitivity = V2 (pi/500) (pi/500)

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 4 4)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = arcBallRotation mouseControlled . arr (0,) . fpsCameraMovement camStartPos wasdControlled

deferredSettingsController :: Num t => YageWire t DeferredSettings DeferredSettings
deferredSettingsController =
  overA voxelDebugModes (hold . popOnEvent (cycle modes) . keyJustReleased Key'F12)
  . overA activeVoxelAmbientOcclusion (toggle (keyJustReleased Key'F9) False True)
  where
  modes = [[VisualizeSceneVoxel], [VisualizePageMask], [VisualizeSceneVoxel, VisualizePageMask], []]

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

instance HasDeferredSettings PBRScene where
  deferredSettings = pbrRenderSettings

instance LinearInterpolatable PBRScene where
  lerp _ _ = id

