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
import Yage.HDR
import Yage.Scene
import Yage.Transformation
import Yage.Pipeline.Deferred
import Yage.Examples.Shared
import qualified Yage.Core.OpenGL as GL


import Yage.UI.GUI

import qualified Yage.Resources as Res
import qualified Yage.Material  as Mat



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

type SceneEntity      = GeoEntityRes
type SceneEnvironment = Environment Light SkyEntityRes
type PBRScene         = Scene HDRCamera SceneEntity SceneEnvironment GUI


pbrTestScene :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () PBRScene
pbrTestScene = proc () -> do
    let initCamera = mkCameraFps (deg2rad 75) (0.1,100.0) idTransformation

    camera       <- cameraControl           -< initCamera

    returnA -< emptyScene (hdrCamera camera) emptyGUI
                    & sceneSky          ?~ sky camera
                    & sceneEntities     .~ [ groundEntity ] ++ spheres
                    & sceneLights       .~ [ mainLight, spotLight01, spotLight02, spotLight03 ]

    where

    hdrCamera cam =
        defaultHDRCamera cam
            & hdrExposure           .~ 2
            & hdrExposureBias       .~ 0.0
            & hdrWhitePoint         .~ 11.2
            & hdrBloomSettings      .~ bloomSettings

    bloomSettings =
        defaultBloomSettings
            & bloomFactor           .~ 0.7
            & bloomPreDownsampling  .~ 2
            & bloomGaussPasses      .~ 5
            & bloomWidth            .~ 2
            & bloomThreshold        .~ 0.5

    sky cam =
        let envPath         = "res" </> "tex" </> "env" </> "Sea" </> "small"
            ext             = "jpg"
            cubeMapFile file= envPath </> file <.> ext

            skyCubeMap      = Mat.mkMaterialF ( Mat.opaque Mat.white ) $ Res.TextureFile <$> Mat.Cube
                                { cubeFaceRight = cubeMapFile "posx", cubeFaceLeft   = cubeMapFile "negx"
                                , cubeFaceTop   = cubeMapFile "posy", cubeFaceBottom = cubeMapFile "negy"
                                , cubeFaceFront = cubeMapFile "posz", cubeFaceBack   = cubeMapFile "negz"
                                }
        in skydome skyCubeMap
            & entityPosition        .~ cam^.cameraLocation
            & entityScale           .~ 100
            & materials
                .Mat.matConfig
                .texConfWrapping
                .texWrapClamping        .~ GL.ClampToEdge
            & materials.Mat.matColor    .~ Mat.opaque (Mat.rgb 2.0 2.0 2.0)

    -- The Ground
    groundEntity :: SceneEntity
    groundEntity =
        ( floorEntity :: SceneEntity )
            & materials         .~ groundMaterial
            & drawSettings      .~ GLDrawSettings GL.Triangles (Just GL.Back)
            & entityPosition    .~ V3 0 (-0.75) 0
            & entityScale       .~ V3 13 1 13

    groundMaterial =
        def & albedoMaterial.Mat.singleMaterial                 .~ TextureFile ( "res" </> "tex" </> "floor_d.png" )
            & albedoMaterial.Mat.matTransformation.transScale   *~ 4.0
            & normalMaterial.Mat.singleMaterial                 .~ TextureFile ( "res" </> "tex" </> "floor_n.png" )
            & normalMaterial.Mat.matTransformation.transScale   *~ 2.0
            & roughnessMaterial.Mat.singleMaterial              .~ TextureFile ( "res" </> "tex" </> "floor_r.png" )
            & normalMaterial.Mat.matTransformation.transScale   *~ 2.0

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


    spheres :: [SceneEntity]
    spheres =
        let sphereEntity    = ( basicEntity :: SceneEntity )
                                & renderData        .~ Res.MeshFile ( "res" </> "model" </> "sphere.ygm", mkSelection [] ) Res.YGMFile
                                & entityOrientation .~ 1
                                & entityScale       .~ 0.5
                                & materials         .~ sphereMaterial
            sphereMaterial  = defaultGeoMaterial
                                & albedoMaterial.Mat.matColor .~ Mat.opaque Mat.dimgray
        in generateOnGrid (7, 7, V2 10 10, sphereEntity)



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

