{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yage
import Yage.Lens hiding ((<.>))
import Yage.Math
import Yage.Wire hiding ((<>))

import Yage.Camera
import Yage.Scene
import Yage.HDR
import Yage.UI.GUI
import Yage.Texture
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
main = yageMain "yage-sponza" appConf winSettings mainWire yDeferredLighting (1/60)


-------------------------------------------------------------------------------
-- View Definition


type SceneEntity      = GeoEntity
type SceneEnvironment = Environment Light SkyEntity
type SponzaScene      = Scene HDRCamera SceneEntity SceneEnvironment GUI

mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () SponzaScene
mainWire = proc () -> do
    world <- worldEntityW -< ()
    cam   <- hdrCameraHandle `overA` cameraControl -< camera
    sky   <- skyDomeW -< cam^.hdrCameraHandle.cameraLocation
    returnA -< emptyScene cam emptyGUI
                    & sceneLights   .~ fromList (genLights baseLight)
                    & sceneEntities .~ fromList [ world ]
                    & sceneSky ?~ sky
    where
    texDir      = "res" </> "tex"

    sponzaModel = meshRes $ loadYGM geoVertex $ ( "res" </> "model" </> "env" </> "sponza.ygm", mkSelection [] )

    worldEntityW =
        let albedoTex = mkTexture2D "Albedo" <$> (imageRes $ texDir</>"default"<.>"png")
            normalTex = mkTexture2D "Normal" <$> (imageRes $ texDir</>"floor_n"<.>"png")
        in proc () -> do
            entity <- renderData <~~ constMeshW sponzaModel
                >>> materials.albedoMaterial.Mat.matTexture <~~ constTextureW albedoTex
                >>> materials.normalMaterial.Mat.matTexture <~~ constTextureW normalTex -< basicEntity :: SceneEntity

            returnA -< entity & materials.albedoMaterial.Mat.stpFactor .~ 2.0
                              & materials.normalMaterial.Mat.stpFactor .~ 2.0
                              & entityScale //~ 200

    baseLight p = Light
                    { _lightType      = Pointlight p 10
                    , _lightColor     = V3 1.0 1.0 1.0
                    , _lightIntensity = 0.1
                    }


    skyDomeW :: YageWire t (V3 Double) SkyEntity
    skyDomeW = proc pos -> do
        tex <- cubeTextureToTexture "SkyCube" . pure <$> constTextureW skyTex -< ()
        returnA -< skydome & materials.skyEnvironmentMap
                                      .Mat.matTexture .~ tex
                           & entityPosition           .~ pos
                           & entityScale              .~ 50

    skyTex  = mkTexture2D "SkyTexture" <$> (imageRes $ texDir</>"misc"</>"blueprint"</>"Seamless Blueprint Textures"</>"1"<.>"png")

    camera          = defaultHDRCamera ( mkCameraFps (deg2rad 75) (0.1,10000) )
                        & hdrExposure           .~ 2
                        & hdrExposureBias       .~ 0.0
                        & hdrWhitePoint         .~ 11.2
                        & hdrBloomSettings      .~ bloomSettings

    bloomSettings   = defaultBloomSettings
                        & bloomFactor           .~ 0.7
                        & bloomPreDownsampling  .~ 2
                        & bloomGaussPasses      .~ 5
                        & bloomWidth            .~ 2
                        & bloomThreshold        .~ 0.5


    genLights mkLight = map mkLight [ V3 (5 * x - 2.5) (5 * y) (5 * z - 2.5)
                                    | x <- [0..2]
                                    , y <- [0..2]
                                    , z <- [0..2]
                                    ]


camStartPos :: V3 Double
camStartPos = V3 0 2 2

mouseSensitivity :: V2 Double
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = fpsCameraMovement camStartPos wasdControlled . fpsCameraRotation mouseControlled

