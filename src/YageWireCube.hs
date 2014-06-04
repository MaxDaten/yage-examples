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
    { windowSize = (800, 600)
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

data CubeView = CubeView
    { _viewCamera     :: CameraHandle
    , _theCube        :: !Cube
    , _lightPosRed    :: !(V3 Float)
    , _lightPosBlue   :: !(V3 Float)
    }
    deriving (Show)

data Cube = Cube
    { _cubePosition    :: !(V3 Float) 
    , _cubeOrientation :: !(Quaternion Float)
    , _cubeScale       :: !(V3 Float)
    }
    deriving (Show)
makeLenses ''Cube


main :: IO ()
main = yageMain "yage-cube" appConf winSettings (simToRender <$> mainWire) yDeferredLighting (1/60)

camStartPos :: V3 Float
camStartPos = V3 0 0 2
mouseSensitivity :: V2 Float
mouseSensitivity = V2 0.1 0.1
cameraKeys :: MovementKeys
cameraKeys = MovementKeys Key'A Key'D Key'W Key'S

mainWire :: (HasTime Float (YageTimedInputState t), Real t) => YageWire t () CubeView
mainWire = proc () -> do
    cubeRot   <- cubeRotationByInput   -< ()
    camera    <- cameraMovement camStartPos cameraKeys . cameraRotation mouseSensitivity -< fpsCamera
    lightPosRed  <- arr (\t-> V3 0 0 (-0.5) + V3 (sin t * 0.5) 0 (cos t * 0.5)) . arr (/2) . time -< () 
    lightPosBlue <- arr (\t-> V3 0 0 (-0.5) + V3 (cos t * 0.5) (sin t) (sin t * 0.5)) . time -< () 
    --lightPos  <- pure (V3 (0) 0 (0.0)) -< () 

    returnA -< CubeView camera
                    (Cube 0 cubeRot 1)
                    (lightPosRed)
                    (lightPosBlue)

    where

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
                            -- & renderData        .~ Res.MeshFile ( "res" </> "model" </> "Cube.ygm" ) Res.YGMFile
                            & entityPosition    .~ (realToFrac <$> _theCube^.cubePosition)
                            & entityOrientation .~ (realToFrac <$> _theCube^.cubeOrientation)
                            & materials.albedoMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "floor_d" <.> ext)
                            & materials.normalMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "floor_n" <.> ext)
                            -- & materials.albedoMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "head" </> "big" </> "head_albedo.jpg")
                            -- & materials.normalMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "head" </> "big" </> "head_tangent.jpg")
            frontLight  = Light ( Pointlight (0 & _z .~ 1.5) 2 ) ( LightAttributes (V4 0.4 0.4 0.4 1) (V3 0 1 (1/64.0)) 15 )

            envPath         = texDir </> "env" </> "Space" </> "small"
            cubeFile file   = envPath </> file <.> ext
            skyCubeMap      = Res.TextureFile <$> Mat.Cube  
                                { cubeFaceRight = cubeFile "posx", cubeFaceLeft  = cubeFile "negx"
                                , cubeFaceTop   = cubeFile "posy", cubeFaceBottom= cubeFile "negy"
                                , cubeFaceFront = cubeFile "posz", cubeFaceBack  = cubeFile "negz"
                                }
            sky             = ( skydome $ Mat.mkMaterialF ( Mat.opaque Mat.white ) skyCubeMap )
                                & transformation.transPosition .~ _viewCamera^.cameraLocation

            theScene        = emptyScene (Camera3D _viewCamera (CameraPlanes 0.1 1000) (deg2rad 75)) 
                                & sceneSky ?~ sky
                                & sceneEnvironment.envAmbient .~ AmbientLight (V3 0.1 0.1 0.1)
        in theScene
            `addEntity` boxE
            `addLight` ( mkLight frontLight )
            