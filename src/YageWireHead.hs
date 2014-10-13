{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleContexts       #-}

module Main where

import Yage
import Yage.Lens hiding ((<.>))
import Yage.Rendering
import Yage.Math
import Yage.Wire hiding ((<>))

import           Yage.Camera
import           Yage.Scene
import           Yage.HDR
import           Yage.UI.GUI
import qualified Yage.Resources as Res
import qualified Yage.Material  as Mat
import           Yage.Pipeline.Deferred
import           Yage.Transformation

import           Yage.Examples.Shared

import qualified Yage.Core.OpenGL as GL


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


main :: IO ()
main = yageMain "yage-head" defaultAppConfig winSettings (simToRender <$> mainWire) yDeferredLighting (1/60)

camStartPos :: V3 Double
camStartPos = V3 0 0.1 1

mouseSensitivity :: V2 Double
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = fpsCameraMovement camStartPos wasdControlled . fpsCameraRotation mouseControlled


mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () HeadView
mainWire = proc () -> do
    let initCamera = mkCameraFps (deg2rad 75) (0.1,1000.0) idTransformation

    headRot   <- headRotationByInput   -< ()
    camera    <- cameraControl -< initCamera
    lightPosRed  <- arr (\t-> V3 0 0 (-0.5) + V3 (sin t * 0.5) 0 (cos t * 0.5)) . arr (/2) . time -< ()
    lightPosBlue <- arr (\t-> V3 0 1 (1) + V3 0.5 0.5 0.5 * V3 (cos t) (sin t) (sin t)) . arr (/2) . time -< ()

    returnA -< HeadView camera
                    (Head 1 headRot 1)
                    (lightPosRed)
                    (lightPosBlue)


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




-------------------------------------------------------------------------------
-- View Definition
type SceneEntity      = GeoEntityRes
type SceneEnvironment = Environment Light SkyEntityRes

simToRender :: HeadView -> Scene HDRCamera SceneEntity SceneEnvironment GUI
simToRender HeadView{..} =
        let
            texDir      = "res" </> "tex"
            objE        = (basicEntity :: GeoEntityRes)
                            & renderData         .~ Res.MeshFile ( "res" </> "model" </> "head.ygm", mkSelection [] ) Res.YGMFile
                            -- & renderData         .~ Res.MeshFile ( "/Users/jloos/Workspace/hs/yage-meta/yage-research/Infinite_Scan_Ver0.1/Infinite-Level_02.OBJ", [] ) Res.OBJFile
                            & entityPosition     .~ V3 0 0.5 0
                            & entityScale        .~ 4
                            & entityOrientation  .~ (realToFrac <$> _theHead^.headOrientation)
                            & materials.albedoMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "head" </> "small" </> "head_albedo.jpg" )
                            & materials.albedoMaterial.Mat.stpFactor._t    %~ negate

                            & materials.normalMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "head" </> "small" </> "head_tangent.jpg" )
                            & materials.normalMaterial.Mat.stpFactor._t    %~ negate

            blackTexture    = mkTexture "SKYE" $ Texture2D $ Mat.pxTexture Mat.TexSRGB8 Mat.black
            skyCubeMap      = Mat.mkMaterialF ( Mat.opaque Mat.white ) $ pure $ Res.TexturePure blackTexture

            sky             = skydome skyCubeMap
                            & entityPosition        .~ _viewCamera^.cameraLocation
                            & entityScale           .~ 100
                            & materials
                                .Mat.matConfig
                                .texConfWrapping
                                .texWrapClamping        .~ GL.ClampToEdge

            frontPLight     = Light
                                { _lightType      = Pointlight (V3 0 0.5 2.5) 10
                                , _lightColor     = V3 1 1 1
                                , _lightIntensity = 1
                                }

            backPLight      = Light
                                { _lightType  = Pointlight (negate (V3 1 1 3)) 30
                                , _lightColor = V3 0.8 0.8 1
                                , _lightIntensity = 1
                                }

            movingPLightRed = Light
                                { _lightType  = Pointlight _lightPosRed 1
                                , _lightColor = V3 1.0 0.0 0.0
                                , _lightIntensity = 1
                                }

            movingPLightBlue= Light
                                { _lightType  = Pointlight _lightPosBlue 1
                                , _lightColor = V3 1.0 1.0 1.0
                                , _lightIntensity = 1
                                }

            bloomSettings   = defaultBloomSettings
                                & bloomFactor           .~ 1
                                & bloomPreDownsampling  .~ 2
                                & bloomGaussPasses      .~ 7
                                & bloomWidth            .~ 2
                                & bloomThreshold        .~ 0.6

            camera          = defaultHDRCamera _viewCamera
                                & hdrExposure           .~ 1
                                & hdrExposureBias       .~ 0.0
                                & hdrWhitePoint         .~ 11.2
                                & hdrBloomSettings      .~ bloomSettings

            theScene        = emptyScene camera emptyGUI
                                & sceneSky ?~ sky
                                & sceneEnvironment.envAmbient .~ AmbientLight 0
        in theScene
            `addEntity` objE

            `addLight` frontPLight
            `addLight` backPLight
            `addLight` movingPLightRed
            `addLight` movingPLightBlue

