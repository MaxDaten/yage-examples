{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Yage hiding (Event, at, key)
import Yage.Rendering
import Yage.Math

import Yage.Examples.Shared

import Control.Wire
import FRP.Netwire.Move




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
    } deriving (Show)
makeLenses ''Cube


main :: IO ()
main = yageMain "MainWire" settings mainWire clockSession

mainWire :: (Real t) => YageWire t () CubeView
mainWire = proc () -> do
    cubeRot   <- cubeRotation   -< ()
    cameraPos <- cameraMovement -< ()
    cameraRot <- cameraRotation -< ()
    returnA -< CubeView 
                    (fpsCamera & cameraLocation    .~ cameraPos
                               & cameraOrientation .~ cameraRot)
                    (Cube (V3 0 0 (-6)) cubeRot 1)

    where

    cubeRotation :: (Real t) => YageWire t a (Quaternion Float)
    cubeRotation = smoothRotation (yAxis, 1) Key'Right 
                  . smoothRotation (yAxis,-1) Key'Left
                  . smoothRotation (xAxis, 1) Key'Up
                  . smoothRotation (xAxis,-1) Key'Down . 1


    cameraMovement :: (Real t) => YageWire t a (V3 Float)
    cameraMovement = V3 <$> integral 0 . velocity (whenKeyDown Key'D)
                        <*> pure 0
                        <*> integral 0 . velocity (whenKeyDown Key'S)

    --proc _ -> do
    --    x <- integral 0 . velocity (whenKeyDown Key'D) -< ()
    --    --y <- integral 0 . velocity (whenKeyDown Key'W) -< ()
    --    z <- integral 0 . velocity (whenKeyDown Key'W) -< ()
    --    returnA -< V3 x 0 z
    cameraRotation = smoothRotation (yAxis, 1) Key'Q 
                  . smoothRotation (yAxis,-1) Key'E . 1
                  -- . smoothRotation (xAxis, 1) Key'Up
                  -- . smoothRotation (xAxis,-1) Key'Down . 1
                    -- (axisAngle xAxis) <$> angle

    smoothRotation :: (Real t) 
                   => (V3 Float, Float) -> Key -> YageWire t (Quaternion Float) (Quaternion Float)
    smoothRotation (axis, dir) key = 
        let rot = (axisAngle axis) <$> integral 0 .  arr (dir *) . velocity (whenKeyDown key)
        in proc inQ -> do
            rotQ <- rot -< ()
            returnA -< inQ * rotQ
        -- 

    velocity :: (Real t) => YageWire t a YageInput -> YageWire t a Float
    velocity whenKey = integrateBounded (0,pi) 0 . (acceleration . whenKey <|> decceleration)

    acceleration :: (Real t) => YageWire t a Float
    acceleration =  pi / 2
    
    decceleration :: (Real t) => YageWire t a Float
    decceleration =  - pi / 2


integrateBounded :: (Ord a, Fractional a, HasTime t s) 
                 => (a,a) -> a -> Wire s e m a a
integrateBounded (lower,upper) x' =
    mkPure $ \ds dx ->
        let dt = realToFrac (dtime ds)
            x  = max lower . min upper $ x' + dt*dx
        in x' `seq` (Right x', integrateBounded (lower,upper) x) 


-- (whenKeyDown Key'Right)
whenKeyDown :: (Real t) => Key -> YageWire t a YageInput
whenKeyDown key = when (\(i, _) -> i `isPressed` key) . currentInputState

currentInputState :: (Num t) => YageWire t a YageInput
currentInputState = mkSF $ \(Timed _ s) _ -> (s, currentInputState)
-------------------------------------------------------------------------------
-- View Definition

instance HasRenderView CubeView where
    getRenderView CubeView{..} = 
        let boxE  = boxEntity & entityPosition .~ _theCube^.cubePosition
                              & entityOrientation .~ _theCube^.cubeOrientation
            scene = emptyRenderScene (Camera3D _viewCamera (deg2rad 60))
                    `addRenderable` boxE
                    `addRenderable` floorEntity
        in RenderUnit scene
            