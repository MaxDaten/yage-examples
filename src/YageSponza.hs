{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Main where

import Yage
import Yage.Lens hiding ((<.>))
import Yage.Math
import Yage.Wire hiding ((<>))

import Yage.Camera
import Yage.Scene
import Yage.HDR
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
main = yageMain "yage-sponza" appConf winSettings (simToRender <$> mainWire) yDeferredLighting (1/60)

mainWire :: (HasTime Float (YageTimedInputState t), Real t) => YageWire t () CubeView
mainWire = 
    let initCamera = mkCameraFps (deg2rad 75) (0.1,100000) idTransformation
    in CubeView <$> cameraControl . pure initCamera
                <*> cubeControl . pure idTransformation
                <*> arr (\t-> V3 0 0 (-0.5) + V3 (sin t * 0.5) 0 (cos t * 0.5)) . arr (/2) . time
                <*> arr (\t-> V3 0 0 (-0.5) + V3 (cos t * 0.5) (sin t) (sin t * 0.5)) . time


camStartPos :: V3 Float
camStartPos = V3 0 2 2

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

simToRender :: CubeView -> Scene HDRCamera SceneEntity SceneEnvironment 
simToRender CubeView{..} = 
    let texDir      = "res" </> "tex"
        ext         = "png"
        boxE        = ( boxEntity :: GeoEntityRes )
                        & renderData              .~ Res.MeshFile ( "res" </> "model" </> "env" </> "sponza.ygm", mkSelection [] ) Res.YGMFile
                        & entityTransformation    .~ _theCube
                        & entityScale             //~ 100
                        & materials.albedoMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir</>"default"<.>"png")
                        & materials.normalMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir</>"floor_n"<.>"png")
                        & materials.traverse.Mat.stpFactor .~ 2.0
        frontLight  = Light Pointlight ( LightAttributes (V4 0.4 0.4 0.4 1) (0, 1, 6) 15 ) 
                        & mkLight
                        & lightRadius   .~ 5


        skyCubeMap      = Res.TextureFile <$> pure (texDir </> "misc" </> "blueprint" </> "Seamless Blueprint Textures" </> "1.png")
        sky             = ( skydome $ Mat.mkMaterialF ( Mat.opaque Mat.white ) skyCubeMap )
                            & entityTransformation.transPosition .~ _viewCamera^.cameraLocation
                            & entityScale .~ 1000

        theScene        = emptyScene (HDRCamera _viewCamera 0.5 1.0 1 (def & bloomFactor .~ 1.0)) 
                            & sceneSky ?~ sky
                            & sceneEnvironment.envAmbient .~ AmbientLight (V3 0.01 0.01 0.01)
    in (foldl' addLight theScene (genLights frontLight))
        `addEntity` boxE
    where
    genLights baseLight = map (\p -> baseLight & lightPosition .~ p ) [ V3 (5 * x - 2.5) (5 * y) (5 * z - 2.5) 
                                                                      | x <- [0..2] 
                                                                      , y <- [0..2] 
                                                                      , z <- [0..2]
                                                                      ] 