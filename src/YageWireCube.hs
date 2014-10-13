{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Yage
import Yage.Lens hiding ((<.>))
import Yage.Math
import Yage.Wire hiding ((<>))

import Yage.Camera
import Yage.Scene
import Yage.HDR
import Yage.UI.GUI
import Yage.Transformation
import qualified Yage.Resources as Res
import qualified Yage.Material  as Mat
import Yage.Pipeline.Deferred
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


appConf :: ApplicationConfig
appConf = defaultAppConfig{ logPriority = WARNING }

type Cube = Transformation Double
data CubeView = CubeView
    { _viewCamera     :: Camera
    , _theCube        :: !Cube
    , _lightPosRed    :: !(V3 Double)
    , _lightPosBlue   :: !(V3 Double)
    }
    deriving (Show)

makeLenses ''CubeView

main :: IO ()
main = yageMain "yage-sponza" appConf winSettings (simToRender <$> mainWire) yDeferredLighting (1/60)

mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () CubeView
mainWire =
    let initCamera = mkCameraFps (deg2rad 75) (0.1,10000) idTransformation
    in CubeView <$> cameraControl . pure initCamera
                <*> cubeControl . pure idTransformation
                <*> arr (\t-> V3 0 0 (-0.5) + V3 (sin t * 0.5) 0 (cos t * 0.5)) . arr (/2) . time
                <*> arr (\t-> V3 0 0 (-0.5) + V3 (cos t * 0.5) (sin t) (sin t * 0.5)) . time


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

cubeControl :: Real t => YageWire t Cube Cube
cubeControl = overA transOrientation cubeRotationByInput

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



-------------------------------------------------------------------------------
-- View Definition


type SceneEntity      = GeoEntityRes
type SceneEnvironment = Environment Light SkyEntityRes

simToRender :: CubeView -> Scene HDRCamera SceneEntity SceneEnvironment GUI
simToRender CubeView{..} =
    let texDir      = "res" </> "tex"
        ext         = "png"
        boxE        = ( boxEntity :: GeoEntityRes )
                        & renderData              .~ Res.MeshFile ( "res" </> "model" </> "Cube.ygm", mkSelection ["face"] ) Res.YGMFile
                        -- & renderData              .~ Res.MeshFile ( "res" </> "model" </> "obj" </> "cube_groups.obj", ["cube"] ) Res.OBJFile
                        -- & renderData              .~ Res.MeshFile ( "res" </> "model" </> "obj" </> "Cube.OBJ", [] ) Res.OBJFile
                        -- & renderData              .~ Res.MeshFile ( "/Users/jloos/Workspace/hs/yage-meta/yage/test/res/cube-textures.obj", ["left", "top", "back", "front", "bottom", "right"] ) Res.OBJFile
                        & entityTransformation    .~ _theCube
                        & entityScale             //~ 2
                        & materials.albedoMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "floor_d" <.> ext)
                        & materials.albedoMaterial.Mat.stpFactor .~ 2.0
                        & materials.normalMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "floor_n" <.> ext)
                        & materials.normalMaterial.Mat.stpFactor .~ 2.0
        frontLight  = Light
                        { _lightType      = Pointlight (V3 25 1 25) 100
                        , _lightColor     = V3 0.4 0.4 0.5
                        , _lightIntensity = 0.1
                        }

        skyCubeMap      = Res.TextureFile <$> pure (texDir </> "misc" </> "blueprint" </> "Seamless Blueprint Textures" </> "1.png")
        sky             = ( skydome $ Mat.mkMaterialF ( Mat.opaque Mat.white ) skyCubeMap )
                            & entityTransformation.transPosition .~ _viewCamera^.cameraLocation
                            & entityScale .~ 50
        bloomSettings   = defaultBloomSettings
                            & bloomFactor           .~ 0.7
                            & bloomPreDownsampling  .~ 2
                            & bloomGaussPasses      .~ 5
                            & bloomWidth            .~ 2
                            & bloomThreshold        .~ 0.5

        camera          = defaultHDRCamera _viewCamera
                            & hdrExposure           .~ 2
                            & hdrExposureBias       .~ 0.0
                            & hdrWhitePoint         .~ 11.2
                            & hdrBloomSettings      .~ bloomSettings

        theScene        = emptyScene camera emptyGUI
                            & sceneSky ?~ sky
                            & sceneEnvironment.envAmbient .~ AmbientLight 0
        in theScene
            `addEntity` boxE
            `addLight` frontLight

