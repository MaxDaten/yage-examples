{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Yage
import Yage.Lens hiding ((<.>))
import Yage.Rendering
import Yage.Math
import Yage.Wire hiding ((<>))

import Yage.Camera
import Yage.HDR
import Yage.Scene
import Yage.Transformation
import Yage.Resources

import Yage.Pipeline.Deferred
import Yage.Pipeline.Deferred.GeometryPass

import Yage.Examples.Shared
import qualified Yage.Core.OpenGL as GL


import Yage.UI.GUI

import qualified Yage.Resources as Res
import qualified Yage.Material  as Mat

import Data.Traversable (sequenceA)


winSettings :: WindowConfig
winSettings = WindowConfig
    { windowSize = (800, 600)
    , windowHints =
        [ WindowHint'ContextVersionMajor  4
        , WindowHint'ContextVersionMinor  1
        , WindowHint'OpenGLProfile        OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat  True
        , WindowHint'RefreshRate          60
        -- , WindowHint'OpenGLDebugContext   True
        -- , WindowHint'Resizable            False
        -- , WindowHint'Decorated            False
        ]
     }


main :: IO ()
main = yageMain "yage-pbr" defaultAppConfig winSettings pbrTestScene yDeferredLighting (1/60)

-------------------------------------------------------------------------------
-- View Definition

type SceneEntity      = GeoEntity
type SceneEnvironment = Environment Light SkyEntity
type PBRScene         = Scene HDRCamera SceneEntity SceneEnvironment GUI


pbrTestScene :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () PBRScene
pbrTestScene = proc () -> do

    hdrCam <- hdrCamera -< ()
    sky    <- skyDomeW  -< hdrCam^.hdrCameraHandle.cameraLocation

    spheres <- spheresW -< ()
    ground  <- overA materials groundMaterialW -< groundEntity

    returnA -< emptyScene hdrCam emptyGUI
                    & sceneSky          ?~ sky
                    & sceneEntities     .~ fromList ( ground:spheres )
                    & sceneLights       .~ fromList [ mainLight, spotLight01, spotLight02, spotLight03 ]

    where
    texDir  = "res"</>"tex"

    sphereMesh = meshResource $ loadYGM geoVertex ("res" </> "model" </> "sphere.ygm", mempty)

    spheresW   = proc _ -> do
        sphere <- pure ( basicEntity :: SceneEntity ) >>> ( renderData <~~ constMeshW sphereMesh ) -< ()
        returnA -< generateOnGrid . (7, 7, V2 10 10,) $
                    sphere & materials.albedoMaterial.Mat.matColor .~ Mat.opaque Mat.dimgray
                           & entityScale //~ 2


    hdrCamera =
        let initCamera = mkCameraFps (deg2rad 75) (0.1,100.0)
        in hdrController . (defaultHDRCamera <$> cameraControl . pure initCamera)

    hdrController =
         hdrExposure      <~~ stepValue 2.0 ( (Key'Period, 0.05), (Key'Comma, 0.05) ) >>>
         hdrExposureBias  <~~ stepValue 0.0 ( (Key'M     , 0.01), (Key'N    , 0.01) ) >>>
         hdrWhitePoint    <~~ pure 11.2       >>>
         hdrBloomSettings <~~ pure bloomSettings

        where

        stepValue initV ((upKey, upStep), (downKey, downStep)) = proc _ -> do
            addVal <- hold . sumE . keyJustPressed upKey    <|> 0 -< upStep
            subVal <- hold . sumE . keyJustPressed downKey  <|> 0 -< downStep
            -- exposureValue <- integral 2.0 . derivative -< addVal - subVal
            returnA -< initV + addVal - subVal


    bloomSettings =
        defaultBloomSettings
            & bloomFactor           .~ 0.7
            & bloomPreDownsampling  .~ 2
            & bloomGaussPasses      .~ 5
            & bloomWidth            .~ 2
            & bloomThreshold        .~ 0.5


    skyDomeW :: YageWire t (V3 Double) SkyEntity
    skyDomeW = proc pos -> do
        tex <- cubeTexture <$> sequenceA (constTextureW <$> skyTex) -< ()
        returnA -< skydome & materials.Mat.matTexture .~ tex
                           & materials.Mat.matTexture
                                .textureConfig
                                .texConfWrapping
                                .texWrapClamping      .~ GL.ClampToEdge
                           & entityPosition           .~ pos
                           & entityScale              .~ 50

    skyTex  =
        let envPath         = "res" </> "tex" </> "env" </> "Sea" </> "small"
            ext             = "jpg"
            fileRes file    = textureResource $ envPath </> file <.> ext
        in Mat.Cube
            { cubeFaceRight = fileRes "posx", cubeFaceLeft   = fileRes "negx"
            , cubeFaceTop   = fileRes "posy", cubeFaceBottom = fileRes "negy"
            , cubeFaceFront = fileRes "posz", cubeFaceBack   = fileRes "negz"
            }
        -- textureResource $ texDir</>"misc"</>"blueprint"</>"Seamless Blueprint Textures"</>"1"<.>"png"

    -- The Ground
    groundEntity :: SceneEntity
    groundEntity =
        ( floorEntity :: SceneEntity )
            -- & materials         .~ groundMaterial
            & drawSettings      .~ GLDrawSettings GL.Triangles (Just GL.Back)
            & entityPosition    .~ V3 0 (-0.75) 0
            & entityScale       .~ V3 13 1 13

    groundMaterialW :: YageWire t GeoMaterial GeoMaterial
    groundMaterialW =
          ( albedoMaterial.Mat.matTexture    <~~ constTextureW ( textureResource $ "res" </> "tex" </> "floor_d.png" )
        >>> normalMaterial.Mat.matTexture    <~~ constTextureW ( textureResource $ "res" </> "tex" </> "floor_n.png" )
        >>> roughnessMaterial.Mat.matTexture <~~ constTextureW ( textureResource $ "res" </> "tex" </> "floor_r.png" ))
        <&> albedoMaterial.Mat.stpFactor   *~ 4.0
        <&> normalMaterial.Mat.stpFactor   *~ 2.0
        <&> normalMaterial.Mat.stpFactor   *~ 2.0

    -- lighting
    mainLight   = makeDirectionalLight (V3 (-1) (-1) 0) (V3 1 0.953 0.918) 0.2

    spotLight01 = makeSpotlight ( V3 8 8 0 )
                                ( V3 (-5) (-5) 0 )
                                50 60
                                ( V3 1 0 0 ) 2

    spotLight02 = makeSpotlight ( V3 (-8) 8 0 )
                                ( V3 5 (-5) 0 )
                                50 60
                                ( V3 0 1 0 ) 1

    spotLight03 = makeSpotlight ( V3 0 8 8 )
                                ( V3 0 (-5) (-5) )
                                50 60
                                ( V3 0.1 0.1 1 ) 1

generateOnGrid :: (Int, Int, V2 Double, SceneEntity) -> [SceneEntity]
generateOnGrid (xCnt, yCnt, V2 dimX dimY, template) =
    map generate (gridIdx `zip` positions)

    where

    generate ((xi, yi), pos) =
        let roughValue  = 0.2 + xi / fromIntegral xCnt
            newSphere   = template & entityPosition .~ pos
                                   & materials.roughnessMaterial.Mat.matColor .~ (realToFrac roughValue)
        in newSphere

    positions   = map calculatePosition gridIdx
    gridIdx     = [ (fromIntegral xi, fromIntegral yi) | xi <- [0 .. xCnt], yi <- [0 .. yCnt] ]
    startPoint  = V3 (-dimX / 2.0) 0 (dimY / 2.0)
    step        = 0 & _x .~  dimX / fromIntegral xCnt
                    & _z .~ -dimY / fromIntegral yCnt
    calculatePosition (xi, yi) =  startPoint + V3 xi 0 yi * step


-- controller wires

camStartPos :: V3 Double
camStartPos = V3 0 1 3

mouseSensitivity :: V2 Double
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = fpsCameraMovement camStartPos wasdControlled . fpsCameraRotation mouseControlled

