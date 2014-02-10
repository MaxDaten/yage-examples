{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Yage hiding (Event, at, key)
import Yage.Rendering
import Yage.Math
import Yage.Wire

import Yage.Examples.Shared

import Data.List
import Control.Monad
import Control.Monad.Random



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



data Particle = Particle
    { _particlePosition    :: V3 Float 
    --, _cubeOrientation :: Quaternion Float
    --, _cubeScale       :: V3 Float
    } deriving (Show)
makeLenses ''Particle


data ParticleSystem = ParticleSystem
    { _pSystemOrigin :: V3 Float
    }
    deriving (Show)
makeLenses ''ParticleSystem


data ParticleView = ParticleView
    { _viewCamera     :: CameraHandle
    , _particles      :: [ParticleSystem]
    }
    deriving (Show)
makeLenses ''ParticleView




main :: IO ()
main = yageMain "yage-particles" settings mainWire clockSession

initialCamOrientation = axisAngle xAxis (deg2rad (-60))

mainWire :: (Real t) => YageWire t () ParticleView
mainWire = proc () -> do
    -- cubeRot   <- cubeRotationByInput   -< ()
    particeSys <- particleSystems -< []
    cameraPos <- cameraMovementByInput -< ()
    cameraRot <- cameraRotationByInput initialCamOrientation -< ()
    returnA -< ParticleView 
                    (fpsCamera & cameraLocation    .~ cameraRot `rotate` (V3 0 1 8 + cameraPos)
                               & cameraOrientation .~ cameraRot)
                    []

    where

    -- stolen from https://github.com/ocharles/netwire-classics/blob/master/asteroids/Asteroids.hs
    particleSystems :: YageWire t [ParticleSystem] [ParticleSystem]
    particleSystems = go []
         where
            go systems = mkGen $ \ds newSystems -> do
                stepped <- mapM (\w -> stepWire w ds (Right ())) systems

                let alive = [ (r, w) | (Right r, w) <- stepped ]
                spawned <- concat <$> mapM spawnParticles newSystems

                return (Right (map fst alive), go $ map snd alive ++ spawned)

            spawnParticles sys = do
                n <- getRandomR (4, 8)
                replicateM n $ do
                  velocity <- randomVelocity (5, 10)
                  life     <- getRandomR (1, 3)
                  return (for life . integral (sys^.pSystemOrigin) . pure velocity)



    randomVelocity :: (Applicative m, MonadRandom m) => (Float, Float) -> m (V3 Float)
    randomVelocity magRange = do
        v <- V3 <$> getRandomR (-1, 1) 
                <*> getRandomR (-1, 1)
                <*> getRandomR (-1, 1)
        mag <- getRandomR magRange
        return (normalize v ^* mag)

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
        in rotationByVelocity upward rightward . arr(/1000) . meassureMouseVelocity (while . keyDown Key'LeftShift)


    -- meassureMouseVelocity :: (Real t) => YageWire t' a a -> YageWire t a (V2 Float)
    meassureMouseVelocity when' = 
        when' . mouseVelocity <|> 0


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
        integrateAttenuated att 0 . (while . keyDown trigger . pure acc <|> 0)


-------------------------------------------------------------------------------
-- View Definition

instance HasRenderView ParticleView where
    getRenderView ParticleView{..} = 
        let floorE  = floorEntity & entityScale .~ 10
            scene   = emptyRenderScene (Camera3D _viewCamera (deg2rad 60))
                        `addRenderable` floorE
        in RenderUnit scene
            