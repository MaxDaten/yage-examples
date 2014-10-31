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
import Yage.Texture
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


main :: IO ()
main = yageMain "yage-material" appConf winSettings mainWire yDeferredLighting (1/60)


-------------------------------------------------------------------------------
-- View Definition


type SceneEntity      = GeoEntity
type SceneEnvironment = Environment Light SkyEntity
type MaterialScene    = Scene HDRCamera SceneEntity SceneEnvironment GUI

mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () MaterialScene
mainWire = proc () -> do
    cam <- hdrCameraHandle `overA` cameraControl -< camera & hdrCameraHandle.cameraOrientation .~ axisAngle (V3 1 0 0) (deg2rad $ -15)
    sky <- skyDomeW -< cam^.hdrCameraHandle.cameraLocation

    gui <- guiWire -< (meshFile, albedoFile, normalFile)

    dummy <- (entityOrientation `overA` previewRotationByInput) . dummyEntityW modelRes albeoTex normalTex -< ()

    returnA -< emptyScene cam gui
                & sceneSky      ?~ sky
                & sceneEntities .~ fromList [ dummy ]
                & sceneLights   .~ fromList [ mainLight, specLight ]


    where
    texDir :: FilePath
    texDir = "res"</>"tex"

    bloomSettings   = defaultBloomSettings
                        & bloomFactor           .~ 0.7
                        & bloomPreDownsampling  .~ 2
                        & bloomGaussPasses      .~ 5
                        & bloomWidth            .~ 2
                        & bloomThreshold        .~ 0.5

    camera          = defaultHDRCamera ( mkCameraFps (deg2rad 75) (0.1,10000) )
                        & hdrExposure           .~ 2
                        & hdrExposureBias       .~ 0.0
                        & hdrWhitePoint         .~ 11.2
                        & hdrBloomSettings      .~ bloomSettings

    meshFile = "res"</>"model"</>"meshpreview"<.>"ygm"
    modelRes = meshResource $ loadYGM geoVertex $ (meshFile, mempty)

    albedoFile = texDir </> "floor_d" <.> "png"
    normalFile = texDir </> "floor_n" <.> "png"
    albeoTex, normalTex :: YageResource Texture
    albeoTex   = textureResource albedoFile
    normalTex  = textureResource normalFile

    dummyEntityW :: YageResource (Mesh GeoVertex) -> YageResource Texture -> YageResource Texture -> YageWire t () GeoEntity
    dummyEntityW meshRes albedoRes normalRes = proc () -> do
        entity <- renderData <~~ constMeshW meshRes
                  >>> materials.albedoMaterial.Mat.matTexture <~~ constTextureW albedoRes
                  >>> materials.normalMaterial.Mat.matTexture <~~ constTextureW normalRes -< boxEntity :: GeoEntity
        returnA -< entity & materials.albedoMaterial.Mat.stpFactor *~ 2.0
                          & materials.normalMaterial.Mat.stpFactor *~ 2.0
                          & entityPosition          -~ V3 0 1 0
                          & entityScale             //~ 200

    mainLight  = Light
                { _lightType      = Pointlight (V3 15 1 15) 100
                , _lightColor     = V3 1.0 1.0 1.0
                , _lightIntensity = 0.5
                }

    specLight  = Light
                { _lightType      = Pointlight (V3 2 1 2) 10
                , _lightColor     = V3 1.0 1.0 1.0
                , _lightIntensity = 1
                }

    skyDomeW :: YageWire t (V3 Double) SkyEntity
    skyDomeW = proc pos -> do
        tex <- cubeTextureToTexture "SkyCube" . pure <$> constTextureW skyTex -< ()
        returnA -< skydome & materials.Mat.matTexture .~ tex
                           & entityPosition           .~ pos
                           & entityScale              .~ 50

    skyTex  = textureResource $ texDir</>"misc"</>"blueprint"</>"Seamless Blueprint Textures"</>"1"<.>"png"

    fontRes = fontResource $ "res"</>"font"</>"yft"</>"SourceCodePro-Regular1024.yft"
    imgRes  = textureResource $ "res"</>"tex"</>"misc"</>"Linear-ZonePlate.png"

    guiWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t (FilePath, FilePath, FilePath) GUI
    guiWire = proc (meshFile, albedoFile, normalFile) -> do
        fontTex <- constFontW fontRes -< ()
        imgTex  <- constTextureW imgRes -< ()
        t       <- time -< ()
        fps     <- avgFps 60 -< ()


        let fileText = emptyTextBuffer fontTex
                        & charColor  .~ V4 0 0 0 1
                        & buffText   .~ format "mesh: {}\nalbedo: {}\nnormal: {}"
                                        ( Shown meshFile, Shown albedoFile, Shown normalFile )

            fileTrans :: Transformation Double
            fileTrans  = idTransformation & transPosition  .~ V3 (15) (750) (1.0)
                                          & transScale._xy .~ 1.1

            timeText = emptyTextBuffer fontTex
                        & charColor  .~ V4 0 0 0 1
                        & buffText  .~ format "t: {}" ( Only $ fixed 2 t )

            timeTrans  = idTransformation & transPosition  .~ V3 (15) (50) (-1.0)
                                          & transScale._xy .~ 1.1
            fpsText  = emptyTextBuffer fontTex
                        & charColor  .~ V4 0 0 0 1
                        & buffText  .~ format "fps: {}" ( Only $ fixed 2 (fps :: Double) )

            fpsTrans  = idTransformation & transPosition  .~ V3 (1030) (50) (-1.0)
                                         & transScale._xy .~ 1.1

        returnA -< emptyGUI & guiElements.at "FileInfo" ?~ GUIFont fileText fileTrans
                            & guiElements.at "Time"     ?~ GUIFont timeText timeTrans
                            & guiElements.at "FPS"      ?~ GUIFont fpsText fpsTrans
                            -- & guiElements.at "Image"    ?~ guiImage imgTex 1 (V2 (10) (10)) (V2 300 780)



-- Camera Control Wires
camStartPos :: V3 Double
camStartPos = V3 0 0 1.5

mouseSensitivity :: V2 Double
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = fpsCameraMovement camStartPos wasdControlled . fpsCameraRotation mouseControlled


-- Dummy Control Wires
-- dummyControl :: Real t => YageWire t Dummy Dummy
-- dummyControl = overA transOrientation dummyRotationByInput

previewRotationByInput :: (Real t) => YageWire t (Quaternion Double) (Quaternion Double)
previewRotationByInput =
    let acc         = 20
        att         = 0.87
    in
   smoothRotationByKey acc att ( yAxis ) Key'Right
 . smoothRotationByKey acc att (-yAxis ) Key'Left
 . smoothRotationByKey acc att ( xAxis ) Key'Up
 . smoothRotationByKey acc att (-xAxis ) Key'Down
