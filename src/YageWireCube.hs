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
import Yage.Rendering
import Yage.Math
import Yage.Wire hiding ((<>))

import Yage.Camera
import Yage.Scene
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

type Cube = Transformation Float
data CubeView = CubeView
    { _viewCamera     :: Camera
    , _theCube        :: !Cube
    , _lightPosRed    :: !(V3 Float)
    , _lightPosBlue   :: !(V3 Float)
    }
    deriving (Show)

makeLenses ''CubeView

main :: IO ()
main = yageMain "yage-cube" appConf winSettings (simToRender <$> mainWire) yDeferredLighting (1/60)

mainWire :: (HasTime Float (YageTimedInputState t), Real t) => YageWire t () CubeView
mainWire = 
    let initCamera = mkCameraFps (deg2rad 75) (0.1,10000) idTransformation
    in CubeView <$> cameraControl . pure initCamera
                <*> cubeControl . pure idTransformation
                <*> arr (\t-> V3 0 0 (-0.5) + V3 (sin t * 0.5) 0 (cos t * 0.5)) . arr (/2) . time
                <*> arr (\t-> V3 0 0 (-0.5) + V3 (cos t * 0.5) (sin t) (sin t * 0.5)) . time


camStartPos :: V3 Float
camStartPos = V3 0 0 2

mouseSensitivity :: V2 Float
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Float)
wasdControlled = wasdMovement (V2 2 2) 

mouseControlled :: Real t => YageWire t () (V2 Float)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = cameraMovement camStartPos wasdControlled . cameraRotation mouseControlled

cubeControl :: Real t => YageWire t Cube Cube
cubeControl = overA transOrientation cubeRotationByInput

cubeRotationByInput :: (Real t) => YageWire t a (Quaternion Float)
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
type SceneEnvironment = Environment LitEntityRes SkyEntityRes

simToRender :: CubeView -> Scene SceneEntity SceneEnvironment 
simToRender CubeView{..} = 
        let texDir      = "res" </> "tex"
            ext         = "png"
            boxE        = ( boxEntity :: GeoEntityRes )
                            & renderData              .~ Res.MeshFile ( "res" </> "model" </> "Cube.ygm" ) Res.YGMFile
                            & entityTransformation    .~ _theCube
                            & entityScale             //~ 2
                            & materials.albedoMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "floor_d" <.> ext)
                            & materials.normalMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "floor_n" <.> ext)
                            -- scale is st tiling factor
                            & materials.traverse.Mat.matTransformation.transScale *~ 2.0
                            -- & materials.traverse.Mat.matTransformation.transOrientation .~ axisAngle (V3 0 0 1) (deg2rad 45)

                            -- & materials.albedoMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "head" </> "big" </> "head_albedo.jpg")
                            -- & materials.normalMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "head" </> "big" </> "head_tangent.jpg")
            frontLight  = Light ( Pointlight (0 & _z .~ 1.5) 4 ) ( LightAttributes (V4 0.4 0.4 0.4 1) (V3 0 1 (1/64.0)) 15 )

            envPath         = texDir </> "env" </> "Space" </> "small"
            cubeFile file   = envPath </> file <.> ext
            skyCubeMap      = Res.TextureFile <$> pure (texDir </> "misc" </> "blueprint" </> "Seamless Blueprint Textures" </> "1.png")
                                --{ cubeFaceRight = cubeFile "posx", cubeFaceLeft  = cubeFile "negx"
                                --, cubeFaceTop   = cubeFile "posy", cubeFaceBottom= cubeFile "negy"
                                --, cubeFaceFront = cubeFile "posz", cubeFaceBack  = cubeFile "negz"
                                --}
            sky             = ( skydome $ Mat.mkMaterialF ( Mat.opaque Mat.white ) skyCubeMap )
                                & entityTransformation.transPosition .~ _viewCamera^.cameraLocation

            theScene        = emptyScene _viewCamera 
                                & sceneSky ?~ sky
                                & sceneEnvironment.envAmbient .~ AmbientLight (V3 0.1 0.1 0.1)
        in theScene
            `addEntity` boxE
            `addLight` ( mkLight frontLight )
            