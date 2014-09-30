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
        --, WindowHint'Resizable            False
        --, WindowHint'Decorated            False
        ]
     }

data SphereView = SphereView
    { _viewCamera     :: Camera
    , _theSphere      :: !Sphere
    }
    deriving (Show)

data Sphere = Sphere
    { _spherePosition    :: !(V3 Float)
    , _sphereOrientation :: !(Quaternion Float)
    , _sphereScale       :: !(V3 Float)
    }
    deriving (Show)
makeLenses ''Sphere


main :: IO ()
main = yageMain "yage-pbr" defaultAppConfig winSettings (simToRender <$> mainWire) yDeferredLighting (1/60)

camStartPos :: V3 Float
camStartPos = V3 0 1 3

mouseSensitivity :: V2 Float
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Float)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Float)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = fpsCameraMovement camStartPos wasdControlled . fpsCameraRotation mouseControlled


mainWire :: (HasTime Float (YageTimedInputState t), Real t) => YageWire t () SphereView
mainWire = proc () -> do
    let initCamera = mkCameraFps (deg2rad 75) (0.1,1000.0) idTransformation

    sphereRot    <- sphereRotationByInput   -< ()
    camera       <- cameraControl           -< initCamera

    returnA -< SphereView camera
                    (Sphere 0 sphereRot 1)

    where

    sphereRotationByInput :: (Real t) => YageWire t a (Quaternion Float)
    sphereRotationByInput =
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
type PBRScene         = Scene HDRCamera SceneEntity SceneEnvironment GUI

--instance HasScene SphereView GeoVertex LitVertex where
simToRender :: SphereView -> PBRScene
simToRender SphereView{..} =
    let sphereEntity    = ( basicEntity :: SceneEntity )
                            & renderData        .~ Res.MeshFile ( "res" </> "model" </> "sphere.ygm", mkSelection [] ) Res.YGMFile
                            & entityOrientation .~ (realToFrac <$> _theSphere^.sphereOrientation)
                            & entityScale       .~ 0.5
                            & materials         .~ sphereMaterial
        sphereMaterial  = defaultGeoMaterial

        -- The Ground
        groundEntity    = ( floorEntity :: SceneEntity )
                            & materials         .~ groundMaterial
                            & drawSettings      .~ GLDrawSettings GL.Triangles (Just GL.Back)
                            & entityPosition    .~ V3 0 (-0.75) 0
                            & entityScale       .~ V3 13 1 13

        groundMaterial  = def & albedoMaterial.Mat.singleMaterial .~ TextureFile ( "res" </> "tex" </> "floor_d.png" )
                              & albedoMaterial.Mat.matTransformation.transScale *~ 2.0
                              & normalMaterial.Mat.singleMaterial .~ TextureFile ( "res" </> "tex" </> "floor_n.png" )
                              & normalMaterial.Mat.matTransformation.transScale *~ 2.0
                              & roughnessMaterial.Mat.singleMaterial .~ TextureFile ( "res" </> "tex" </> "floor_r.png" )
                              & normalMaterial.Mat.matTransformation.transScale *~ 8.0

        -- lighting
        mainLight       = Light Pointlight ( LightAttributes 1 (0, 0, 1.0/64) 64 )
                            & mkLight
                            & lightPosition .~ V3 0 15 10
                            & lightRadius   .~ 50
        secondLight     = Light Pointlight ( LightAttributes 1 (0, 0, 1.0/1024) 1024 )
                            & mkLight
                            & lightPosition .~ V3 (-15) 15 (-10)
                            & lightRadius   .~ 50

        softLight       = Light Pointlight ( LightAttributes 1 (0, 0, 1.0/64) 16 )
                            & mkLight
                            & lightPosition .~ V3 10 15 (-10)
                            & lightRadius   .~ 50


        --envPath         = "res" </> "tex" </> "env" </> "RomeChurch" </> "small"
        --envPath         = "res" </> "tex" </> "env" </> "RomeChurch" </> "big"
        envPath         = "res" </> "tex" </> "env" </> "Sea" </> "small"
        ext             = "jpg"
        cubeMapFile file= envPath </> file <.> ext

        --skyCubeMap      = Res.TextureFile <$> pure ("res" </> "tex" </> "misc" </> "blueprint" </> "Seamless Blueprint Textures" </> "1.png")
        --skyCubeMap      = Res.TextureFile <$> pure (cubeMapFile "posx")

        skyCubeMap      = Mat.mkMaterialF ( Mat.opaque Mat.white ) $ Res.TextureFile <$> Mat.Cube
                            { cubeFaceRight = cubeMapFile "posx", cubeFaceLeft   = cubeMapFile "negx"
                            , cubeFaceTop   = cubeMapFile "posy", cubeFaceBottom = cubeMapFile "negy"
                            , cubeFaceFront = cubeMapFile "posz", cubeFaceBack   = cubeMapFile "negz"
                            }

        sky             = skydome skyCubeMap
                            & entityPosition        .~ _viewCamera^.cameraLocation
                            & entityScale           .~ 100
                            & materials
                                .Mat.matConfig
                                .texConfWrapping
                                .texWrapClamping    .~ GL.ClampToEdge

        bloomSettings   = defaultBloomSettings
                            & bloomFactor           .~ 1
                            & bloomPreDownsampling  .~ 2
                            & bloomGaussPasses      .~ 6

        camera          = defaultHDRCamera _viewCamera
                            & hdrExposure           .~ 2
                            & hdrExposureBias       .~ 0.0
                            & hdrWhitePoint         .~ 11.2
                            & hdrBloomSettings      .~ bloomSettings

        theScene        = emptyScene camera emptyGUI
                            & sceneSky ?~ sky
                            & sceneEnvironment.envAmbient .~ AmbientLight 0
    in theScene
        `addSpheres` (7, 7, V2 10 10, sphereEntity)
        `addEntity` groundEntity
        `addLight` mainLight
        `addLight` secondLight
        `addLight` softLight


addSpheres :: PBRScene -> (Int, Int, V2 Float, SceneEntity) -> PBRScene
addSpheres scene (xCnt, yCnt, V2 dimX dimY, sphere)
    = foldr addSphereOnGrid scene (gridIdx `zip` positions)

    where

    addSphereOnGrid ((xi, yi), pos) onScene =
        let roughValue  = 0.2 + xi / fromIntegral xCnt
            newSphere   = sphere & entityPosition .~ pos
                                 & materials.roughnessMaterial.Mat.matColor .~ (realToFrac roughValue)
        in onScene `addEntity` newSphere

    positions   = map calculatePosition gridIdx
    gridIdx     = [ (fromIntegral xi, fromIntegral yi) | xi <- [0 .. xCnt], yi <- [0 .. yCnt] ]
    startPoint  = V3 (-dimX / 2.0) 0 (dimY / 2.0)
    step        = 0 & _x .~  dimX / fromIntegral xCnt
                    & _z .~ -dimY / fromIntegral yCnt
    calculatePosition (xi, yi) =  startPoint + V3 xi 0 yi * step
