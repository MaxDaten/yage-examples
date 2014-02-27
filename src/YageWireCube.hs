{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Yage
import Yage.Lens
import Yage.Rendering
import Yage.Math
import Yage.Wire hiding ((<>))

import Yage.Camera
import Yage.Scene
import Yage.Pipeline.Deferred
import Yage.Examples.Shared
import Yage.Resources




settings :: WindowConfig
settings = WindowConfig
    { windowSize = (800, 600)
    , windowHints = 
        [ WindowHint'ContextVersionMajor  3
        , WindowHint'ContextVersionMinor  2
        , WindowHint'OpenGLProfile        OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat  True
        , WindowHint'RefreshRate          60
        --, WindowHint'Resizable            False
        --, WindowHint'Decorated            False
        ]
     }

data CubeView = CubeView
    { _viewCamera     :: CameraHandle
    , _theCube        :: Cube
    }
    deriving (Show)

data Cube = Cube
    { _cubePosition    :: V3 Float 
    , _cubeOrientation :: Quaternion Float
    , _cubeScale       :: V3 Float
    }
    deriving (Show)
makeLenses ''Cube


main :: IO ()
main = yageMain "yage-cube" settings mainWire clockSession

startPos :: V3 Float
startPos = V3 0 2.3 5

mainWire :: (Real t) => YageWire t () CubeView
mainWire = proc () -> do
    cubeRot   <- cubeRotationByInput   -< ()
    cameraPos <- cameraMovementByInput -< ()
    cameraRot <- cameraRotationByInput -< ()
    returnA -< CubeView 
                    (fpsCamera & cameraLocation    .~ cameraRot `rotate` (startPos + cameraPos)
                               & cameraOrientation .~ cameraRot)
                    (Cube (V3 0 0.5 0) cubeRot 1)

    where

    cubeRotationByInput :: (Real t) => YageWire t a (Quaternion Float)
    cubeRotationByInput =
          smoothRotationByKey (yAxis, 1) Key'Right 
        . smoothRotationByKey (yAxis,-1) Key'Left
        . smoothRotationByKey (xAxis, 1) Key'Up
        . smoothRotationByKey (xAxis,-1) Key'Down 
        . 1



    cameraMovementByInput :: (Real t) => YageWire t a (V3 Float)
    cameraMovementByInput = 
        let acc         = 20
            att         = 0.8
            toLeft      = -xAxis
            toRight     =  xAxis
            forward     = -zAxis
            backward    =  zAxis
        in smoothTranslation forward  acc att Key'W
         . smoothTranslation toLeft   acc att Key'A
         . smoothTranslation backward acc att Key'S
         . smoothTranslation toRight  acc att Key'D
         . 0



    smoothTranslation :: (Real t)
                      => V3 Float -> Float -> Float -> Key -> YageWire t (V3 Float) (V3 Float)
    smoothTranslation dir acc att key =
        let trans = integral 0 . arr (signorm dir ^*) . velocity acc att key
        in proc inTransV -> do
            transV <- trans -< ()
            returnA -< inTransV + transV


    velocity :: (Floating b, Ord b, Real t) 
             => b -> b -> Key -> YageWire t a b
    velocity acc att trigger = 
        integrateAttenuated att 0 . (whileKeyDown trigger . pure acc <|> 0)


---------------------------------------------------------------------------------------------------


    cameraRotationByInput :: (Real t) => YageWire t a (Quaternion Float)
    cameraRotationByInput =
        let upward                  = V3 0 1 0
            rightward               = V3 1 0 0
        in rotationByVelocity upward rightward . arr(/1000) . (whileKeyDown Key'LeftShift . mouseVelocity <|> 0)


    smoothRotationByKey :: (Real t) 
                        => (V3 Float, Float) -> Key -> YageWire t (Quaternion Float) (Quaternion Float)
    smoothRotationByKey (axis, dir) key = 
        let acc         = 20
            att         = 0.87
            angleVel    = velocity acc att key
            rot         = axisAngle axis <$> integral 0 . arr (dir*) . angleVel
        in proc inQ -> do
            rotQ    <- rot -< ()
            returnA -< inQ * rotQ


    rotationByVelocity :: (Real t) => V3 Float -> V3 Float -> YageWire t (V2 Float) (Quaternion Float)
    rotationByVelocity xMap yMap = 
        let applyOrientations   = arr (axisAngle xMap . (^._x)) &&& arr (axisAngle yMap . (^._y))
            combineOrientations = arr (\(qu, qr) -> qu * qr)
        in combineOrientations . applyOrientations . integral 0


-------------------------------------------------------------------------------
-- View Definition

instance HasScene CubeView GeoVertex where
    getScene CubeView{..} = 
        let boxE        = boxEntity 
                            & entityPosition    .~ V3 0 0 (-3) --(realToFrac <$> _theCube^.cubePosition)
                            & entityOrientation .~ (realToFrac <$> _theCube^.cubeOrientation)
            
            sphereE     = sphereEntity 2
                            & entityPosition    .~ V3 (-3) 0.5 0
                            & entityOrientation .~ (realToFrac <$> _theCube^.cubeOrientation)

            coneE       = coneEntity 12
                            & entityPosition    .~ V3 3 0 0
                            & entityOrientation .~ (realToFrac <$> _theCube^.cubeOrientation)
            pyramidE    = pyramidEntity
                            & entityPosition    .~ V3 0 0 3
                            & entityOrientation .~ (realToFrac <$> _theCube^.cubeOrientation)
            floorE      = floorEntity & entityScale .~ 10
            objE        = objEntity (OBJResource ("res" </> "obj" </> "head.obj") (undefined))
                            & entityPosition     .~ V3 0 2 0
                            & entityScale        *~ 2
        in emptyScene (Camera3D _viewCamera (CameraPlanes 0.1 1000) (deg2rad 60))
            `addEntity` boxE
            `addEntity` sphereE
            `addEntity` coneE
            `addEntity` pyramidE
            `addEntity` floorE
            `addEntity` objE
            