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
import Yage.Scene
import Yage.Pipeline.Deferred
import Yage.Examples.Shared

settings :: WindowConfig
settings = WindowConfig
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
    { _viewCamera     :: CameraHandle
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
main = yageMain "yage-cube" settings mainWire (1/60)

camStartPos :: V3 Float
camStartPos = V3 0 0 2
mouseSensitivity :: V2 Float
mouseSensitivity = V2 0.1 0.1
cameraKeys :: MovementKeys
cameraKeys = MovementKeys Key'A Key'D Key'W Key'S

mainWire :: (HasTime Float (YageTimedInputState t), Real t) => YageWire t () SphereView
mainWire = proc () -> do
    sphereRot    <- sphereRotationByInput   -< ()
    camera       <- cameraMovement camStartPos cameraKeys . cameraRotation mouseSensitivity -< fpsCamera

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

instance HasScene SphereView GeoVertex LitVertex where
    getScene SphereView{..} = 
        let 
            sphereE         = objEntity ( OBJResource $ "res" </> "model" </> "sphere.obj" )
                                & entityOrientation .~ (realToFrac <$> _theSphere^.sphereOrientation)

            envPath         = "res" </> "tex" </> "env" </> "RomeChurch" </> "small"
            ext             = "png"
            cubeMapFile file= envPath </> file <.> ext
            sky             = ( skydome $ CubeMap { cubeFaceRight = cubeMapFile "posx", cubeFaceLeft  = cubeMapFile "negx"
                                                  , cubeFaceTop   = cubeMapFile "posy", cubeFaceBottom= cubeMapFile "negy"
                                                  , cubeFaceFront = cubeMapFile "posz", cubeFaceBack  = cubeMapFile "negz"
                                                  }
                              ) & skyPosition .~ _viewCamera^.cameraLocation & skyIntensity .~ 0.8
            theScene        = emptyScene (Camera3D _viewCamera (CameraPlanes 0.1 1000) (deg2rad 75)) 
                                & sceneSky ?~ sky
                                & sceneEnvironment.envAmbient .~ AmbientLight (V3 0.1 0.1 0.1)
        in theScene
            `addEntity` sphereE
            