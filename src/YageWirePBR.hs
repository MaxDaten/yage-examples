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

data SphereView = SphereView
    { _viewCamera     :: Camera
    , _theSphere      :: !Sphere
    }
    deriving (Show)

data Sphere = Sphere
    { _spherePosition    :: !(V3 Double)
    , _sphereOrientation :: !(Quaternion Double)
    , _sphereScale       :: !(V3 Double)
    }
    deriving (Show)
makeLenses ''Sphere


main :: IO ()
main = yageMain "yage-pbr" defaultAppConfig winSettings (simToRender <$> mainWire) yDeferredLighting (1/60)

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


mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () SphereView
mainWire = proc () -> do
    let initCamera = mkCameraFps (deg2rad 75) (0.1,100.0) idTransformation

    sphereRot    <- sphereRotationByInput   -< ()
    camera       <- cameraControl           -< initCamera

    returnA -< SphereView camera
                    (Sphere 0 sphereRot 1)

    where

    sphereRotationByInput :: (Real t) => YageWire t a (Quaternion Double)
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
type SceneEnvironment = Environment Light SkyEntityRes
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
                            & albedoMaterial.Mat.matColor .~ Mat.opaque Mat.dimgray

        -- The Ground
        groundEntity    = ( floorEntity :: SceneEntity )
                            & materials         .~ groundMaterial
                            & drawSettings      .~ GLDrawSettings GL.Triangles (Just GL.Back)
                            & entityPosition    .~ V3 0 (-0.75) 0
                            & entityScale       .~ V3 13 1 13

        groundMaterial  = def & albedoMaterial.Mat.singleMaterial .~ TextureFile ( "res" </> "tex" </> "floor_d.png" )
                              & albedoMaterial.Mat.matTransformation.transScale *~ 4.0
                              & normalMaterial.Mat.singleMaterial .~ TextureFile ( "res" </> "tex" </> "floor_n.png" )
                              & normalMaterial.Mat.matTransformation.transScale *~ 2.0
                              & roughnessMaterial.Mat.singleMaterial .~ TextureFile ( "res" </> "tex" </> "floor_r.png" )
                              & normalMaterial.Mat.matTransformation.transScale *~ 2.0

        -- lighting
        mainLight       = makeDirectionalLight (V3 (-1) (-1) 0) (V3 1 0.953 0.918) 0.2

        spotLight01       = makeSpotlight ( V3 8 8 0 )
                                          ( V3 (-5) (-5) 0 )
                                          50 60
                                          ( V3 1 0 0 ) 2

        spotLight02       = makeSpotlight ( V3 (-8) 8 0 )
                                          ( V3 5 (-5) 0 )
                                          50 60
                                          ( V3 0 1 0 ) 1

        spotLight03       = makeSpotlight ( V3 0 8 8 )
                                          ( V3 0 (-5) (-5) )
                                          50 60
                                          ( V3 0.1 0.1 1 ) 1


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
                                .texWrapClamping        .~ GL.ClampToEdge
                            & materials.Mat.matColor    .~ Mat.opaque (Mat.rgb 2.0 2.0 2.0)

        bloomSettings   = defaultBloomSettings
                            & bloomFactor           .~ 0.7
                            & bloomPreDownsampling  .~ 2
                            & bloomGaussPasses      .~ 5
                            & bloomWidth            .~ 2
                            & bloomThreshold        .~ 0.5

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
        `addLight` spotLight01
        `addLight` spotLight02
        `addLight` spotLight03
        -- `addLight` secondLight
        -- `addLight` softLight


addSpheres :: PBRScene -> (Int, Int, V2 Double, SceneEntity) -> PBRScene
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
