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

data CubeView = CubeView
    { _viewCamera     :: CameraHandle
    , _theCube        :: !Cube
    , _lightPosRed    :: !(V3 Float)
    , _lightPosBlue   :: !(V3 Float)
    }
    deriving (Show)

data Cube = Cube
    { _cubePosition    :: !(V3 Float) 
    , _cubeOrientation :: !(Quaternion Float)
    , _cubeScale       :: !(V3 Float)
    }
    deriving (Show)
makeLenses ''Cube


main :: IO ()
main = yageMain "yage-cube" settings mainWire (1/60)

camStartPos :: V3 Float
camStartPos = V3 0 0 2
mouseSensitivity :: V2 Float
mouseSensitivity = V2 0.1 0.1
cameraKeys :: MovementKeys
cameraKeys = MovementKeys Key'A Key'D Key'W Key'S

mainWire :: (HasTime Float (YageTimedInputState t), Real t) => YageWire t () CubeView
mainWire = proc () -> do
    cubeRot   <- cubeRotationByInput   -< ()
    camera    <- cameraMovement camStartPos cameraKeys . cameraRotation mouseSensitivity -< fpsCamera
    lightPosRed  <- arr (\t-> V3 0 0 (-0.5) + V3 (sin t * 0.5) 0 (cos t * 0.5)) . arr (/2) . time -< () 
    lightPosBlue <- arr (\t-> V3 0 0 (-0.5) + V3 (cos t * 0.5) (sin t) (sin t * 0.5)) . time -< () 
    --lightPos  <- pure (V3 (0) 0 (0.0)) -< () 

    returnA -< CubeView camera
                    (Cube 1 cubeRot 1)
                    (lightPosRed)
                    (lightPosBlue)

    where

    cubeRotationByInput :: (Real t) => YageWire t a (Quaternion Float)
    cubeRotationByInput =
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

instance HasScene CubeView GeoVertex LitVertex where
    getScene CubeView{..} = 
        let 
            objE        = objEntity ( YGMResource $ "res" </> "model" </> "head.ygm" )
                            & entityPosition     .~ V3 0 0.5 (-0.5)
                            & entityScale        *~ 5
                            & entityOrientation .~ (realToFrac <$> _theCube^.cubeOrientation)
                            & textures           .~ [ Left ("res" </> "tex" </> "head" </> "big" </> "head_albedo.jpg")
                                                    --, TextureDefinition (1, "tex_normal")  $ TextureFile ("res" </> "tex" </> "head_normal.jpg")
                                                    , Left ("res" </> "tex" </> "head" </> "big" </> "head_tangent.jpg")
                                                    ]
            frontPLAttr     = LightAttributes (V4 0.2 0.2 0.2 1) (V3 0 1 (1/64.0)) 15
            backPLAttr      = LightAttributes (V4 0.3 0.3 0.5 1) (V3 1 0 (1/128.0)) 30
            movingAttrRed   = LightAttributes (V4 0.4 0.2 0.2 1) (V3 1 1 (1/64.0)) 15 
            movingAttrBlue  = LightAttributes (V4 0.2 0.2 0.4 1) (V3 1 1 (1/64.0)) 15 
            
            frontPLight     = mkLight $ Light (Pointlight ((V3 0 0.5 2)) 5) frontPLAttr
            backPLight      = mkLight $ Light (Pointlight ((V3 (-1) (-1) (-3))) 5) backPLAttr
            movingPLightRed = mkLight $ Light (Pointlight (realToFrac <$> _lightPosRed) 0.5) movingAttrRed
            movingPLightBlue= mkLight $ Light (Pointlight (realToFrac <$> _lightPosBlue) 0.5) movingAttrBlue
            theScene        = emptyScene (Camera3D _viewCamera (CameraPlanes 0.1 1000) (deg2rad 75))
                                & sceneEnvironment.envAmbient .~ AmbientLight (V3 0.1 0.1 0.1)
        in theScene
            `addEntity` objE
            
            `addLight` frontPLight
            `addLight` backPLight
            `addLight` movingPLightRed
            `addLight` movingPLightBlue
            