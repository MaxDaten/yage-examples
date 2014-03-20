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
import Yage.Light
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

mainWire :: (HasTime Float (YageTimedInputState t), Real t) => YageWire t () CubeView
mainWire = proc () -> do
    cubeRot   <- cubeRotationByInput   -< ()
    camera    <- cameraMovement . cameraRotation -< fpsCamera
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

instance HasScene CubeView GeoVertex LitVertex where
    getScene CubeView{..} = 
        let 
            objE        = objEntity (YGMResource ("res" </> "model" </> "head.ygm") (undefined))
                            & entityPosition     .~ V3 0 0.5 (-0.5)
                            & entityScale        *~ 5
                            & entityOrientation .~ (realToFrac <$> _theCube^.cubeOrientation)
                            & textures           .~ [ Left ("res" </> "tex" </> "head" </> "big" </> "head_albedo.jpg")
                                                    --, TextureDefinition (1, "tex_normal")  $ TextureFile ("res" </> "tex" </> "head_normal.jpg")
                                                    , Left ("res" </> "tex" </> "head" </> "big" </> "head_tangent.jpg")
                                                    ]
            frontPLAttr     = LightAttributes 0 (V4 1 0.9 0.9 1) (V4 0.2 0.2 0.2 1) (V3 0 1 (1/64.0)) 15
            backPLAttr      = LightAttributes 0 (V4 0.5 0.5 0.8 1) (V4 0.3 0.3 0.3 1) (V3 1 0 (1/128.0)) 30
            movingAttrRed   = LightAttributes 0 (V4 0.7 0.3 0.3 1) (V4 0.4 0.2 0.2 1) (V3 1 1 (1/64.0)) 15 
            movingAttrBlue  = LightAttributes 0 (V4 0.3 0.3 0.7 1) (V4 0.4 0.2 0.2 1) (V3 1 1 (1/64.0)) 15 
            
            frontPLight     = mkLight $ Light (Pointlight ((V3 0 0.5 2)) 5) frontPLAttr
            backPLight      = mkLight $ Light (Pointlight ((V3 (-1) (-1) (-3))) 5) backPLAttr
            movingPLightRed = mkLight $ Light (Pointlight (realToFrac <$> _lightPosRed) 0.5) movingAttrRed
            movingPLightBlue= mkLight $ Light (Pointlight (realToFrac <$> _lightPosBlue) 0.5) movingAttrBlue

        in (emptyScene (Camera3D _viewCamera (CameraPlanes 0.1 1000) (deg2rad 75)))
            `addEntity` objE
            
            `addLight` frontPLight
            `addLight` backPLight
            `addLight` movingPLightRed
            `addLight` movingPLightBlue
            