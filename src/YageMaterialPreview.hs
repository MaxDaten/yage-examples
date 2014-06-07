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

type Dummy = Transformation Float

data MaterialView = MaterialView
    { _viewCamera     :: Camera
    , _dummy          :: !Dummy
    } deriving (Show)

makeLenses ''MaterialView


main :: IO ()
main = yageMain "yage-material" appConf winSettings (simToRender <$> mainWire) yDeferredLighting (1/60)


mainWire :: (HasTime Float (YageTimedInputState t), Real t) => YageWire t () MaterialView
mainWire =
    let initCamera = mkCameraFps (deg2rad 75) (0.1,10000) $ idTransformation & transOrientation .~ axisAngle (V3 1 0 0) (deg2rad $ -15)
    -- warning, camera init position will be overidden by cam starting pos (we integrate the position in the camerMovement wire)
    in MaterialView <$> cameraControl . pure initCamera
                    <*> dummyControl . pure idTransformation


-- Camera Control Wires
camStartPos :: V3 Float
camStartPos = V3 0 0 1.5

mouseSensitivity :: V2 Float
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Float)
wasdControlled = wasdMovement (V2 2 2) 

mouseControlled :: Real t => YageWire t () (V2 Float)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = cameraMovement camStartPos wasdControlled . cameraRotation mouseControlled


-- Dummy Control Wires
dummyControl :: Real t => YageWire t Dummy Dummy
dummyControl = overA transOrientation dummyRotationByInput

dummyRotationByInput :: (Real t) => YageWire t (Quaternion Float) (Quaternion Float)
dummyRotationByInput =
    let acc         = 20
        att         = 0.87
    in 
   smoothRotationByKey acc att ( yAxis ) Key'Right 
 . smoothRotationByKey acc att (-yAxis ) Key'Left
 . smoothRotationByKey acc att ( xAxis ) Key'Up
 . smoothRotationByKey acc att (-xAxis ) Key'Down



-------------------------------------------------------------------------------
-- View Definition


type SceneEntity      = GeoEntityRes
type SceneEnvironment = Environment LitEntityRes SkyEntityRes

simToRender :: MaterialView -> Scene SceneEntity SceneEnvironment 
simToRender MaterialView{..} = 
        let texDir      = "res" </> "tex"
            ext         = "png"
            boxE        = ( boxEntity :: GeoEntityRes )
                            & renderData              .~ Res.MeshFile ( "/Users/jloos/Workspace/hs/yage-meta/yage-examples/res/model/meshpreview.ygm" ) Res.YGMFile
                            & entityTransformation    .~ _dummy
                            & entityPosition          -~ V3 0 1 0
                            & entityScale             //~ 200
                            & materials.albedoMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "floor_d" <.> ext)
                            & materials.normalMaterial.Mat.singleMaterial .~ ( Res.TextureFile $ texDir </> "floor_n" <.> ext)
                            -- scale is st tiling factor
                            & materials.traverse.Mat.matTransformation.transScale *~ 2.0

            frontLight  = Light Pointlight ( LightAttributes (V4 0.4 0.4 0.4 1) (0, 1, 1/64.0) 15 ) 
                            & mkLight
                            & lightPosition .~ V3 0 0 1.5
                            & lightRadius   .~ 4

            skyCubeMap      = Res.TextureFile <$> pure (texDir </> "misc" </> "blueprint" </> "Seamless Blueprint Textures" </> "1.png")
            sky             = ( skydome $ Mat.mkMaterialF ( Mat.opaque Mat.white ) skyCubeMap )
                                & entityTransformation.transPosition .~ _viewCamera^.cameraLocation

            theScene        = emptyScene _viewCamera 
                                & sceneSky ?~ sky
                                & sceneEnvironment.envAmbient .~ AmbientLight (V3 0.1 0.1 0.1)
        in theScene
            `addEntity` boxE
            `addLight` frontLight
            