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
import Yage.Texture
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
    { windowSize = (1600, 1000)
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


pbrTestScene :: (HasTime Double (YageTimedInputState t), Real t, Show t) => YageWire t () PBRScene
pbrTestScene = proc () -> do

    hdrCam <- hdrCamera -< ()
    sky    <- skyDomeW  -< hdrCam^.hdrCameraHandle.cameraLocation

    spheres <- spheresW -< ()
    ground  <- overA materials groundMaterialW -< groundEntity

    returnA -< emptyScene hdrCam emptyGUI
                    & sceneSky          ?~ sky
                    & sceneEntities     .~ fromList ( ground:spheres )
                    & sceneLights       .~ fromList [ mainLight {--, spotLight01, spotLight02, spotLight03 --} ]

    where
    texDir  = "res"</>"tex"

    sphereMesh = meshRes $ loadYGM geoVertex ("res" </> "model" </> "sphere.ygm", mempty)

    spheresW   = proc _ -> do
        sphere <- pure ( basicEntity :: SceneEntity ) >>> ( renderData <~~ constMeshW sphereMesh ) -< ()
        returnA -< generateOnGrid . (10, 1, V2 10 1, 2, ) $
                    sphere & materials.albedoMaterial.Mat.matColor .~ Mat.opaque Mat.gold
                           & entityScale //~ 2


    hdrCamera =
        let initCamera = mkCameraFps (deg2rad 75) (0.1,100.0)
        in hdrController . (defaultHDRCamera <$> cameraControl . pure initCamera)

    hdrController =
         hdrExposure      <~~ ( spin (-10, 10) 2.0 <<< (( 0.05 <$) <$> keyJustPressed Key'Period)  &&&
                                                       ((-0.05 <$) <$> keyJustPressed Key'Comma) ) >>>

         hdrExposureBias  <~~ ( spin (-10, 10) 0.0 <<< (( 0.01 <$) <$> keyJustPressed Key'M)       &&&
                                                       ((-0.01 <$) <$> keyJustPressed Key'N) )     >>>
         hdrWhitePoint    <~~ pure 11.2       >>>
         hdrBloomSettings <~~ pure bloomSettings


    bloomSettings =
        defaultBloomSettings
            & bloomFactor           .~ 0.7
            & bloomPreDownsampling  .~ 1
            & bloomGaussPasses      .~ 5
            & bloomWidth            .~ 1
            & bloomThreshold        .~ 0.5


    skyDomeW :: YageWire t (V3 Double) SkyEntity
    skyDomeW = proc pos -> do
        -- tex <- cubeTextureToTexture "SkyCube" <$> sequenceA (constTextureW <$> skyTex) -< ()
        tex <- constTextureW skyTex -< ()
        returnA -< skydome & materials.skyEnvironmentMap
                                      .Mat.matTexture .~ tex
                           & materials.skyRadianceMap
                                      .Mat.matTexture .~ tex
                           & materials.skyEnvironmentMap
                                .Mat.matTexture
                                .textureConfig
                                .texConfWrapping
                                .texWrapClamping      .~ GL.ClampToEdge
                           & entityPosition           .~ pos
                           & entityScale              .~ 50

    skyTex        = mkTextureCubeMip "SeaCross" <$>
                        cubeCrossMipsRes Strip (texDir</>"env"</>"Sea"</>"pmrem"</>"*_m<->.png")
                            <&> textureConfig.texConfWrapping.texWrapClamping .~ GL.ClampToEdge
    -- skyTex  =
    --     let envPath         = "res" </> "tex" </> "env" </> "Sea" </> "small"
    --         ext             = "jpg"
    --         fileRes file    = mkTexture2D (fromString . fpToString $ file) <$> (imageRes $ envPath </> file <.> ext)
    --     in Mat.Cube
    --         { cubeFaceRight = fileRes "posx", cubeFaceLeft   = fileRes "negx"
    --         , cubeFaceTop   = fileRes "posy", cubeFaceBottom = fileRes "negy"
    --         , cubeFaceFront = fileRes "posz", cubeFaceBack   = fileRes "negz"
    --         }
    --     -- textureResource $ texDir</>"misc"</>"blueprint"</>"Seamless Blueprint Textures"</>"1"<.>"png"

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
          ( albedoMaterial.Mat.matTexture    <~~ constTextureW ( mkTexture2D "FloorD" <$> (imageRes $ "res" </> "tex" </> "floor_d.png") )
        >>> normalMaterial.Mat.matTexture    <~~ constTextureW ( mkTexture2D "FloorN" <$> (imageRes $ "res" </> "tex" </> "floor_n.png") )
        >>> roughnessMaterial.Mat.matTexture <~~ constTextureW ( mkTexture2D "FloorR" <$> (imageRes $ "res" </> "tex" </> "floor_r.png") ))
        <&> albedoMaterial.Mat.stpFactor   *~ 4.0
        <&> normalMaterial.Mat.stpFactor   *~ 2.0
        <&> normalMaterial.Mat.stpFactor   *~ 2.0

    -- lighting
    mainLight   = makeDirectionalLight (V3 (0) (-1) (-1)) (V3 1 0.953 0.918) 0.75

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

generateOnGrid :: (Int, Int, V2 Double, Double, SceneEntity) -> [SceneEntity]
generateOnGrid (xCnt, yCnt, V2 dimX dimY, scale, template) =
    map generate (gridIdx `zip` positions)

    where

    generate ((xi, yi), pos) =
        let roughValue  = xi / fromIntegral (xCnt-1)
            newSphere   = template & entityPosition .~ pos
                                   & materials.roughnessMaterial.Mat.matColor .~ (realToFrac roughValue)
        in newSphere

    positions   = map calculatePosition gridIdx
    gridIdx     = [ (fromIntegral xi, fromIntegral yi) | xi <- [0 .. xCnt-1], yi <- [0 .. yCnt-1] ]
    startPoint  = (step + V3 (-dimX) 0 (dimY)) ^* 0.5
    step        = 0 & _x .~  dimX / fromIntegral xCnt
                    & _z .~ -dimY / fromIntegral yCnt
    calculatePosition (xi, yi) =  startPoint + V3 xi 0 yi * step


-- controller wires

camStartPos :: V3 Double
camStartPos = V3 0 1 5

mouseSensitivity :: V2 Double
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = arcBallRotation mouseControlled . arr (0,) . fpsCameraMovement camStartPos wasdControlled

