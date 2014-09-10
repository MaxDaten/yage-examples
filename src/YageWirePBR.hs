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
camStartPos = V3 0 0 2

mouseSensitivity :: V2 Float
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Float)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Float)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = cameraMovement camStartPos wasdControlled . cameraRotation mouseControlled


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


--instance HasScene SphereView GeoVertex LitVertex where
simToRender :: SphereView -> Scene HDRCamera SceneEntity SceneEnvironment GUI
simToRender SphereView{..} =
    let sphereE         = ( boxEntity :: SceneEntity )
                            & renderData        .~ Res.MeshFile ( "res" </> "model" </> "sphere.ygm", mkSelection [] ) Res.YGMFile
                            & entityOrientation .~ (realToFrac <$> _theSphere^.sphereOrientation)

        envPath         = "res" </> "tex" </> "env" </> "RomeChurch" </> "small"
        ext             = "png"

        mainLight       = Light Pointlight ( LightAttributes 1 (0, 1.0/20, 1.0/132) 64 )
                            & mkLight
                            & lightPosition .~ V3 0 0 1.5
                            & lightRadius   .~ 3

        softLight       = Light Pointlight ( LightAttributes 1 (0, 0, 1.0/900) 32 )
                            & mkLight
                            & lightPosition .~ V3 10 1 10
                            & lightRadius   .~ 100

        cubeMapFile file= envPath </> file <.> ext
        skyCubeMap      = Res.TextureFile <$> pure ("res" </> "tex" </> "misc" </> "blueprint" </> "Seamless Blueprint Textures" </> "1.png")

        --skyCubeMap      = Res.TextureFile <$> Mat.Cube
        --                    { cubeFaceRight = cubeMapFile "posx", cubeFaceLeft   = cubeMapFile "negx"
        --                    , cubeFaceTop   = cubeMapFile "posy", cubeFaceBottom = cubeMapFile "negy"
        --                    , cubeFaceFront = cubeMapFile "posz", cubeFaceBack   = cubeMapFile "negz"
        --                    }
        sky             = ( skydome $ Mat.mkMaterialF ( Mat.opaque Mat.white ) skyCubeMap )
                            & entityPosition .~ _viewCamera^.cameraLocation
                            & entityScale    .~ 50

        camera          = HDRCamera _viewCamera 1.0 1.0 1.0 ( def & bloomFactor .~ 1.0 )
        theScene        = emptyScene camera emptyGUI
                            & sceneSky ?~ sky
                            & sceneEnvironment.envAmbient .~ AmbientLight 0
    in theScene
        `addEntity` sphereE
        --`addLight` mainLight
        `addLight` softLight

