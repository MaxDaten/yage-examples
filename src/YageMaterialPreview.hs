{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Main where

import Yage
import Yage.Lens hiding ((<.>))
import Yage.Math
import Yage.Wire hiding ((<>), at)

import Yage.Camera
import Yage.Scene
import Yage.HDR
import Yage.Texture.TextureAtlas
import Yage.Formats.Font

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

type Dummy = Transformation Float

data MaterialView = MaterialView
    { _viewCamera     :: Camera
    , _dummy          :: !Dummy
    , _gui            :: GUI
    }

makeLenses ''MaterialView


main :: IO ()
main = yageMain "yage-material" appConf winSettings (simToRender <$> mainWire) yDeferredLighting (1/60)


mainWire :: (HasTime Float (YageTimedInputState t), Real t) => YageWire t () MaterialView
mainWire =
    let initCamera = mkCameraFps (deg2rad 75) (0.1,10000) $ idTransformation & transOrientation .~ axisAngle (V3 1 0 0) (deg2rad $ -15)
    -- warning, camera init position will be overidden by cam starting pos (we integrate the position in the camerMovement wire)
    in MaterialView <$> cameraControl . pure initCamera
                    <*> dummyControl . pure idTransformation
                    <*> guiWire


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
cameraControl = fpsCameraMovement camStartPos wasdControlled . fpsCameraRotation mouseControlled


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


fontPath :: FilePath
fontPath  = "res" </> "font" </> "yft" </> "SourceCodePro-Regular1024.yft"

guiWire :: Real t => YageWire t () GUI
guiWire = proc _ -> do
    fontTex <- hold . once . now . loadExternalFont -< ()
    t       <- time -< ()

    let infoTxt  = format "mesh: {}\nalbedo: {}\nnormal: {}"
                    ( Shown meshFile, Shown albeoFile, Shown normalFile )

        fileText = emptyTextBuffer fontTex
                    & charColor  .~ V4 0 0 0 1
                    & buffText  .~ infoTxt

        fileTrans  = idTransformation & transPosition  .~ V3 50 180 0
                                      & transScale._xy *~ 1.5

        timeTxt  = format "t: {}"
                    ( Only $ fixed 2 t )

        timeText = emptyTextBuffer fontTex
                    & charColor  .~ V4 0 0 0 1
                    & buffText  .~ timeTxt

        timeTrans  = idTransformation & transPosition  .~ V3 1000 50 0
                                      & transScale._xy *~ 1.5

    returnA -< emptyGUI & guiElements.at "FileInfo" ?~ GUIFont fileText fileTrans
                        & guiElements.at "Time"     ?~ GUIFont timeText timeTrans

    where
    loadExternalFont = mkGenN $ \_ -> do
        fontTex <- readFontTexture fontPath
        return $ (Right fontTex, mkConst $ Right fontTex)

-------------------------------------------------------------------------------
-- View Definition


type SceneEntity      = GeoEntityRes
type SceneEnvironment = Environment LitEntityRes SkyEntityRes


texDir :: FilePath
texDir      = "res" </> "tex"

albeoFile, normalFile, meshFile :: FilePath
albeoFile   = texDir </> "floor_d" <.> "png"
normalFile  = texDir </> "floor_n" <.> "png"
meshFile    = "res" </> "model" </> "meshpreview" <.> "ygm"

simToRender :: MaterialView -> Scene HDRCamera SceneEntity SceneEnvironment GUI
simToRender MaterialView{..} =
        let boxE        = ( boxEntity :: GeoEntityRes )
                            & renderData              .~ Res.MeshFile ( meshFile, mkSelection [] ) Res.YGMFile
                            & entityTransformation    .~ _dummy
                            & entityPosition          -~ V3 0 1 0
                            & entityScale             //~ 200
                            & materials.albedoMaterial.Mat.singleMaterial .~ Res.TextureFile albeoFile
                            & materials.albedoMaterial.Mat.stpFactor .~ 2.0
                            & materials.normalMaterial.Mat.singleMaterial .~ Res.TextureFile normalFile
                            & materials.normalMaterial.Mat.stpFactor .~ 2.0

            mainLight  = Light Pointlight ( LightAttributes 1 (0, 1.0/30, 1.0/900) 64 )
                            & mkLight
                            & lightPosition .~ V3 15 1 15
                            & lightRadius   .~ 100
            specLight  = Light Pointlight ( LightAttributes 1 (0, 1.0/30, 1.0/20) 128 )
                            & mkLight
                            & lightPosition .~ V3 2 1 2
                            & lightRadius   .~ 10

            skyCubeMap      = Res.TextureFile <$> pure (texDir </> "misc" </> "blueprint" </> "Seamless Blueprint Textures" </> "1.png")
            sky             = ( skydome $ Mat.mkMaterialF ( Mat.opaque Mat.white ) skyCubeMap )
                                & entityPosition .~ _viewCamera^.cameraLocation
                                & entityScale    .~ 50

            camera          = HDRCamera _viewCamera 1 1.0 1.0 (def & bloomFactor .~ 1)

            theScene        = emptyScene camera _gui
                                & sceneSky ?~ sky
                                & sceneEnvironment.envAmbient .~ AmbientLight 0
        in theScene
            `addEntity` boxE
            `addLight` mainLight
            `addLight` specLight

