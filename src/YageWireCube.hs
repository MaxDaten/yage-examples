{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Traversable (sequenceA)

import Yage
import Yage.Lens hiding ((<.>))
import Yage.Math
import Yage.Wire hiding ((<>))

import Yage.Camera
import Yage.Scene
import Yage.HDR
import Yage.Texture
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
main = yageMain "yage-cube" appConf winSettings mainWire yDeferredLighting (1/60)


type SceneEnvironment = Environment Light SkyEntity

type CubeScene = Scene HDRCamera GeoEntity SceneEnvironment GUI

mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () CubeScene
mainWire = proc _ -> do

    cam    <- overA hdrCameraHandle cameraControl -< camera
    sky    <- skyDomeW -< cam^.hdrCameraHandle.cameraLocation

    boxEntity <- boxEntityW >>> (entityOrientation <~~ cubeRotationByInput) -< ()

    returnA -< emptyScene cam emptyGUI
                & sceneSky      ?~ sky
                & sceneEntities .~ fromList [ boxEntity ]
                & sceneLights   .~ fromList [ frontLight ]

    where
    texDir  = "res"</>"tex"

    frontLight  = Light
                    { _lightType      = Pointlight (V3 0 1 25) 100
                    , _lightColor     = 1
                    , _lightIntensity = 50
                    }


    boxEntityW :: YageWire t b GeoEntity
    boxEntityW =
        let albedoTex = mkTexture2D "FloorD" <$> (imageRes $ texDir</>"floor_d"<.>"png")
            normalTex = mkTexture2D "FloorN" <$> (imageRes $ texDir</>"floor_n"<.>"png")
        in (pure (basicEntity :: GeoEntity)
                >>> renderData <~~ constMeshW boxMesh
                >>> materials.albedoMaterial.Mat.matTexture <~~ constTextureW albedoTex
                >>> materials.normalMaterial.Mat.matTexture <~~ constTextureW normalTex)
                <&> materials.albedoMaterial.Mat.stpFactor .~ 2.0
                <&> materials.normalMaterial.Mat.stpFactor .~ 2.0
                <&> entityScale //~ 2


    boxMesh :: YageResource (Mesh GeoVertex)
    boxMesh = meshRes $ loadYGM geoVertex ( "res" </> "model" </> "Cube.ygm", mkSelection ["face"] )

    skyDomeW :: YageWire t (V3 Double) SkyEntity
    skyDomeW = proc pos -> do
        tex <- cubeTextureToTexture "SkyCube" . pure <$> constTextureW skyTex -< ()
        returnA -< skydome & materials.Mat.matTexture .~ tex
                           & entityPosition           .~ pos
                           & entityScale              .~ 50

    skyTex  = mkTexture2D "SkyBlueprint" <$> (imageRes $ texDir</>"misc"</>"blueprint"</>"Seamless Blueprint Textures"</>"1"<.>"png")

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
