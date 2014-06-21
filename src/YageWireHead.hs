{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleContexts       #-}

module Main where

import Yage
import Yage.Lens hiding ((<.>))
import Yage.Rendering
import Yage.Math
import Yage.Wire hiding ((<>))

import           Yage.Camera
import           Yage.Scene
import qualified Yage.Resources as Res
import           Yage.Material            hiding (Head)
import           Yage.Pipeline.Deferred
import           Yage.Examples.Shared

winSettings :: WindowConfig
winSettings = WindowConfig
    { windowSize = (1200, 800)
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

data HeadView = HeadView
    { _viewCamera     :: !Camera
    , _theHead        :: !Head
    , _lightPosRed    :: !(V3 Float)
    , _lightPosBlue   :: !(V3 Float)
    }
    deriving (Show)

data Head = Head
    { _headPosition    :: !(V3 Float) 
    , _headOrientation :: !(Quaternion Float)
    , _headScale       :: !(V3 Float)
    }
    deriving (Show)

makeLenses ''HeadView
makeLenses ''Head


main :: IO ()
main = yageMain "yage-head" defaultAppConfig winSettings (simToRender <$> mainWire) yDeferredLighting (1/60)

camStartPos :: V3 Float
camStartPos = V3 0 0 1

mouseSensitivity :: V2 Float
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Float)
wasdControlled = wasdMovement (V2 2 2) 

mouseControlled :: Real t => YageWire t () (V2 Float)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = cameraMovement camStartPos wasdControlled . cameraRotation mouseControlled


mainWire :: (HasTime Float (YageTimedInputState t), Real t) => YageWire t () HeadView
mainWire = proc () -> do
    let initCamera = mkCameraFps (deg2rad 75) (0.1,1000.0) idTransformation
    
    headRot   <- headRotationByInput   -< ()
    camera    <- cameraControl -< initCamera
    lightPosRed  <- arr (\t-> V3 0 0 (-0.5) + V3 (sin t * 0.5) 0 (cos t * 0.5)) . arr (/2) . time -< () 
    lightPosBlue <- arr (\t-> V3 0 0 (-0.5) + V3 (cos t * 0.5) (sin t) (sin t * 0.5)) . time -< () 

    returnA -< HeadView camera
                    (Head 1 headRot 1)
                    (lightPosRed)
                    (lightPosBlue)


headRotationByInput :: (Real t) => YageWire t a (Quaternion Float)
headRotationByInput =
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

simToRender :: HeadView -> Scene SceneEntity SceneEnvironment 
simToRender HeadView{..} = 
        let 
            texDir      = "res" </> "tex"
            objE        = (basicEntity :: GeoEntityRes)
                            & renderData         .~ Res.MeshFile ( "res" </> "model" </> "head.ygm", mkSelection [] ) Res.YGMFile
                            -- & renderData         .~ Res.MeshFile ( "/Users/jloos/Workspace/hs/yage-meta/yage-research/Infinite_Scan_Ver0.1/Infinite-Level_02.OBJ", [] ) Res.OBJFile
                            & entityPosition     .~ V3 0 0.5 0
                            & entityScale        .~ 4
                            & entityOrientation  .~ (realToFrac <$> _theHead^.headOrientation)
                            & materials.albedoMaterial.singleMaterial .~ ( Res.TextureFile $ texDir </> "head" </> "small" </> "head_albedo.jpg" )
                            & materials.normalMaterial.singleMaterial .~ ( Res.TextureFile $ texDir </> "head" </> "small" </> "head_tangent.jpg" )
                            & materials.traverse.stpFactor._t %~ negate

            frontPLight     = Light Pointlight (LightAttributes (V4 0.2 0.2 0.2 1) (0, 1, 1/64.0  ) 15)
                                & mkLight & lightPosition .~ V3 0 0.5 1
                                          & lightRadius   .~ 5
            backPLight      = Light Pointlight (LightAttributes (V4 0.3 0.3 0.5 1) (1, 0, 1/128.0 ) 30)
                                & mkLight & lightPosition .~ negate (V3 1 1 3)
                                          & lightRadius   .~ 5
            movingPLightRed = Light Pointlight (LightAttributes (V4 0.4 0.2 0.2 1) (1, 1, 1/64.0  ) 15)
                                & mkLight & lightPosition .~ (realToFrac <$> _lightPosRed)
                                          & lightRadius   .~ 0.5
            movingPLightBlue= Light Pointlight (LightAttributes (V4 0.2 0.2 0.4 1) (1, 1, 1/64.0  ) 15)
                                & mkLight & lightPosition .~ (realToFrac <$> _lightPosBlue)
                                          & lightRadius   .~ 0.5
            theScene        = emptyScene _viewCamera
                                & sceneEnvironment.envAmbient .~ AmbientLight (V3 0.01 0.01 0.01)
        in theScene
            `addEntity` objE
            
            `addLight` frontPLight
            `addLight` backPLight
            `addLight` movingPLightRed
            `addLight` movingPLightBlue
            