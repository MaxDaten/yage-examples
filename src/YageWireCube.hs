{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

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
    , _theCube        :: !Cube
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
camStartPos = V3 0 2.3 5
mouseSensitivity :: V2 Float
mouseSensitivity = V2 0.1 0.1

mainWire :: (Real t) => YageWire t () CubeView
mainWire = proc () -> do
    cubeRot   <- cubeRotationByInput   -< ()
    camera    <- cameraMovement . cameraRotation -< fpsCamera

    returnA -< CubeView camera
                    (Cube 1 cubeRot 1)

    where

    cubeRotationByInput :: (Real t) => YageWire t a (Quaternion Float)
    cubeRotationByInput =
       smoothRotationByKey ( yAxis ) Key'Right 
     . smoothRotationByKey (-yAxis ) Key'Left
     . smoothRotationByKey ( xAxis ) Key'Up
     . smoothRotationByKey (-xAxis ) Key'Down 
     . 1



    cameraMovement :: (Real t) => YageWire t (CameraHandle) (CameraHandle)
    cameraMovement =
        let acc         = 2
            toLeft      = -xAxis
            toRight     =  xAxis
            forward     = -zAxis
            backward    =  zAxis
        in proc cam -> do
            leftA      <- pure ( toLeft   ) . whileKeyDown Key'A <|> 0 -< ()
            rightA     <- pure ( toRight  ) . whileKeyDown Key'D <|> 0 -< ()
            forwardA   <- pure ( forward  ) . whileKeyDown Key'W <|> 0 -< ()
            backwardA  <- pure ( backward ) . whileKeyDown Key'S <|> 0 -< ()
            let trans  = leftA + rightA + forwardA + backwardA
                r      = (cam^.cameraOrientation)
            transV  <- integral camStartPos -< acc *^ normalize $ r `rotate` trans 
            returnA -< (cam & cameraLocation .~ transV)


    cameraRotation :: (Real t) => YageWire t (CameraHandle) (CameraHandle)
    cameraRotation =
        proc cam -> do
            velV <- arr ((-mouseSensitivity) * ) . (whileKeyDown Key'LeftShift . mouseVelocity <|> 0) -< () -- counter clock wise
            x    <- integral 0                   -< velV^._x
            y    <- integrateBounded (-90, 90) 0 -< velV^._y
            returnA -< cam `pan`  x
                           `tilt` y




    smoothTranslation :: (Real t)
                      => V3 Float -> Float -> Float -> Key -> YageWire t (V3 Float) (V3 Float)
    smoothTranslation dir acc att key =
        let trans = integral 0 . arr (signorm dir ^*) . velocity acc att key
        in proc inTransV -> do
            transV <- trans -< ()
            returnA -< inTransV + transV 

    velocity :: (Floating b, Ord b, Real t) 
             => b -> b -> Key -> YageWire t a b
    velocity !acc !att !trigger = 
        integrateAttenuated att 0 . (pure acc . whileKeyDown trigger <|> 0)


    smoothRotationByKey :: (Real t) 
                        => V3 Float -> Key -> YageWire t (Quaternion Float) (Quaternion Float)
    smoothRotationByKey !axis !key = 
        let acc         = 20
            att         = 0.87
            angleVel    = velocity acc att key
            rot         = axisAngle axis <$> integral 0 . angleVel
        in proc inQ -> do
            rotQ    <- rot -< ()
            returnA -<  inQ * rotQ -- * conjugate rotQ

---------------------------------------------------------------------------------------------------




    rotationByVelocity :: (Real t) => V3 Float -> V3 Float -> YageWire t (V2 Float) (Quaternion Float)
    rotationByVelocity !xMap !yMap =
        let applyOrientations   = arr (axisAngle xMap . (^._x)) &&& arr (axisAngle yMap . (^._y))
            combineOrientations = arr (\(!qu, !qr) -> qu * qr)
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
            