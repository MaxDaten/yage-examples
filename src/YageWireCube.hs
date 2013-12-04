{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Yage hiding (Event, at, key)
import Yage.Rendering
import Yage.Math

import Yage.Examples.Shared

import Data.Foldable
import Control.Wire
import Control.Wire.Unsafe.Event
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

initialCamOrientation = axisAngle xAxis (deg2rad (-60))

mainWire :: (Real t) => YageWire t () CubeView
mainWire = proc () -> do
    cubeRot   <- cubeRotationByInput   -< ()
    cameraPos <- cameraMovementByInput -< ()
    cameraRot <- cameraRotationByInput initialCamOrientation -< ()
    returnA -< CubeView 
                    (fpsCamera & cameraLocation    .~ cameraRot `rotate` (V3 0 1 8 + cameraPos)
                               & cameraOrientation .~ cameraRot)
                    (Cube (V3 0 1 0) cubeRot 1)

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
            toRight     = xAxis
            forward     = -zAxis
            backward    = zAxis
        in smoothTranslation forward  acc att Key'W
         . smoothTranslation toLeft   acc att Key'A
         . smoothTranslation backward acc att Key'S
         . smoothTranslation toRight  acc att Key'D
         . 0


    cameraRotationByInput :: (Real t) => Quaternion Float -> YageWire t a (Quaternion Float)
    cameraRotationByInput initial =
        let upward                  = V3 0 1 0
            rightward               = V3 1 0 0
        in rotationByVelocity upward rightward . arr(/1000) . arr traceShow' . meassureMouseVelocity

    meassureMouseVelocity :: (Real t) => YageWire t a (V2 Float)
    meassureMouseVelocity = 
        while . keyDown Key'LeftShift . mouseVelocity <|> 0


    smoothTranslation :: (Real t)
                       => V3 Float -> Float -> Float -> Key -> YageWire t (V3 Float) (V3 Float)
    smoothTranslation dir acc att key =
        let trans = integral 0 . arr (signorm dir ^*) . velocity acc att key
        in proc inTransV -> do
            transV <- trans -< ()
            returnA -< inTransV + transV


    smoothRotationByKey :: (Real t) 
                   => (V3 Float, Float) -> Key -> YageWire t (Quaternion Float) (Quaternion Float)
    smoothRotationByKey (axis, dir) key = 
        let acc         = 20
            att         = 0.85
            angleVel    = velocity acc att key
            rot         = axisAngle axis <$> integral 0 . arr (dir*) . angleVel
        in proc inQ -> do
            rotQ <- rot -< ()
            returnA -< inQ * rotQ


    rotationByVelocity :: (Real t) => V3 Float -> V3 Float -> YageWire t (V2 Float) (Quaternion Float)
    rotationByVelocity xMap yMap = 
        let applyOrientations   = arr (axisAngle xMap . (^._x)) &&& arr (axisAngle yMap . (^._y))
            combineOrientations = arr (\(qu, qr) -> qu * qr)
        in combineOrientations . applyOrientations . integral 0


    velocity :: (Floating b, Ord b, Real t) 
             => b -> b -> Key -> YageWire t a b
    velocity acc att trigger = 
        integrateAttenuated att 0 . (while . keyDown trigger . pure acc <|> 0) --- --> pure acc <|> pure decc)




mouseVelocity :: (Real t) => YageWire t a (V2 Float)
mouseVelocity = derivativeF . arr traceShow' . currentMousePosition


mouseAcceleration :: (Real t) => YageWire t a (V2 Float)
mouseAcceleration = derivativeF . mouseVelocity


while :: YageWire t (Event a) a
while = mkPure_ $ event (Left mempty) Right

keyDown :: (Real t) => Key -> YageWire t a (Event a)
keyDown key = 
    mkSF $ \(Timed _ (i, _)) x ->
        if i `isPressed` key
            then (Event x, keyDown key)
            else (NoEvent, keyDown key)

whenKeyUp :: (Real t) => Key -> YageWire t a YageInput
whenKeyUp key = arr traceShow' . unless (\(i, _) -> i `isPressed` key) . currentInputState


currentMousePosition :: (Real t, Fractional b) => YageWire t a (V2 b)
currentMousePosition = mkSF_ (\(inputSt,_) -> realToFrac <$> (uncurry V2 $ inputSt^.mouse.mousePosition)) . currentInputState

currentInputState :: (Num t) => YageWire t a YageInput
currentInputState = mkSF $ \(Timed _ s) _ -> (s, currentInputState)



-------------------------------------------------------------------------------
-- Generic Wires


integrateAttenuated :: (Floating a, Ord a, HasTime t s) 
                 => a -> a -> Wire s e m a a
integrateAttenuated a@attenuation x' = mkPure $ \ds dx ->
    let dt = realToFrac (dtime ds)
        x  = a * (x' + dt * dx)
    in x' `seq` ( Right x', integrateAttenuated a x )


integrateBounded :: (Floating a, Ord a, HasTime t s) 
                 => (a, a) -> a -> Wire s e m a a
integrateBounded b@(lower,upper) x' = mkPure $ \ds dx ->
    let dt = realToFrac (dtime ds)
        x  = x' + dt * dx
        n  = min upper . max lower $ x
    in x' `seq` ( Right x', integrateBounded b n )



derivativeF :: (Foldable f, Fractional (f a), RealFloat a, HasTime t s, Monoid e)
            => Wire s e m (f a) (f a)
derivativeF = mkPure $ \_ x -> (Left mempty, loop x)
    where
    loop x' = 
        mkPure $ \ds x ->
            let dt  = realToFrac (dtime ds)
                dx  = (x - x') / dt
                mdx | any isNaN dx       = Right 0
                    | any isInfinite dx  = Left mempty
                    | otherwise          = Right dx
            in mdx `seq` (mdx, loop x)



-------------------------------------------------------------------------------
-- View Definition

instance HasRenderView CubeView where
    getRenderView CubeView{..} = 
        let boxE    = boxEntity & entityPosition    .~ _theCube^.cubePosition
                                & entityOrientation .~ _theCube^.cubeOrientation
            floorE  = floorEntity & entityScale .~ 10
            scene   = emptyRenderScene (Camera3D _viewCamera (deg2rad 60))
                        `addRenderable` boxE
                        `addRenderable` floorE
        in RenderUnit scene
            