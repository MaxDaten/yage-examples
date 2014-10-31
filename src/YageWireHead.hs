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
import           Yage.HDR
import           Yage.Texture
import           Yage.UI.GUI
import qualified Yage.Resources as Res
import qualified Yage.Material  as Mat
import           Yage.Pipeline.Deferred
import           Yage.Transformation

import           Yage.Examples.Shared

import qualified Yage.Core.OpenGL as GL


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
    , _lightPosRed    :: !(V3 Double)
    , _lightPosBlue   :: !(V3 Double)
    }
    deriving (Show)

data Head = Head
    { _headPosition    :: !(V3 Double)
    , _headOrientation :: !(Quaternion Double)
    , _headScale       :: !(V3 Double)
    }
    deriving (Show)

makeLenses ''HeadView
makeLenses ''Head


main :: IO ()
main = yageMain "yage-head" defaultAppConfig winSettings mainWire yDeferredLighting (1/60)

-------------------------------------------------------------------------------
-- View Definition
type SceneEntity      = GeoEntity
type SceneEnvironment = Environment Light SkyEntity
type HeadScene        = Scene HDRCamera SceneEntity SceneEnvironment GUI


mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () HeadScene
mainWire = proc () -> do

    cam    <- overA hdrCameraHandle cameraControl -< camera
    sky    <- skyDomeW -< cam^.hdrCameraHandle.cameraLocation

    headEntity <- headEntityW >>> (entityOrientation <~~ headRotationByInput) -< ()

    redLight  <- pLightRedW  -< ()
    blueLight <- pLightBlueW -< ()

    returnA -< emptyScene cam emptyGUI
                & sceneSky      ?~ sky
                & sceneEntities .~ fromList [ headEntity ]
                & sceneLights   .~ fromList [ frontPLight, backPLight, redLight, blueLight ]

    where
    texDir      = "res" </> "tex"

    skyTex :: YageResource Texture
    skyTex = pure $ mkTexture2D "SKYE" $ Mat.pxTexture Mat.TexSRGB8 Mat.black

    skyDomeW :: YageWire t (V3 Double) SkyEntity
    skyDomeW = proc pos -> do
        tex <- cubeTextureToTexture "SkyCube" . pure <$> constTextureW skyTex -< ()
        returnA -< skydome & materials.Mat.matTexture .~ tex
                           & entityPosition           .~ pos
                           & entityScale              .~ 50

    frontPLight     = Light
                        { _lightType        = Pointlight (V3 0 0.5 5) 20
                        , _lightColor       = V3 1 1 1
                        , _lightIntensity   = 1
                        }
    backPLight      = Light
                        { _lightType        = Pointlight (negate (V3 1 1 3)) 30
                        , _lightColor       = V3 0.8 0.8 1
                        , _lightIntensity   = 1
                        }
    pLightRedW =
        Light <$> (Pointlight <$> arr (\t-> V3 0 0 (-0.5) + V3 (sin t * 0.5) 0 (cos t * 0.5)) . arr (/2) . time
                              <*> pure 1)
              <*> (pure $ V3 1.0 0.0 0.0)
              <*> (pure 0.1)

    pLightBlueW =
        Light <$> (Pointlight <$> arr (\t-> V3 0 1 (1) + V3 0.5 0.5 0.5 * V3 (cos t) (sin t) (sin t)) . arr (/2) . time
                              <*> pure 1)
              <*> (pure $ V3 0.85 0.85 1.0)
              <*> (pure 0.1)

    headEntityW :: YageWire t b GeoEntity
    headEntityW =
        let albedoTex = textureResource $ texDir</>"head"</>"small"</>"head_albedo.jpg"
            normalTex = textureResource $ texDir</>"head"</>"small"</>"head_tangent.jpg"
        in (pure (basicEntity :: GeoEntity)
                >>> renderData <~~ constMeshW headMesh
                >>> materials.albedoMaterial.Mat.matTexture <~~ constTextureW albedoTex
                >>> materials.normalMaterial.Mat.matTexture <~~ constTextureW normalTex)
                <&> materials.albedoMaterial.Mat.stpFactor %~ negate
                <&> materials.normalMaterial.Mat.stpFactor %~ negate
                <&> entityScale         .~ 4
                <&> entityPosition      .~ V3 0 0.5 0

    headMesh :: YageResource (Mesh GeoVertex)
    headMesh = meshResource $ loadYGM geoVertex ( "res" </> "model" </> "head.ygm", mkSelection [] )

    bloomSettings   = defaultBloomSettings
                        & bloomFactor           .~ 1
                        & bloomPreDownsampling  .~ 2
                        & bloomGaussPasses      .~ 7
                        & bloomWidth            .~ 2
                        & bloomThreshold        .~ 0.6

    camera          = defaultHDRCamera ( mkCameraFps (deg2rad 75) (0.1,1000.0) )
                        & hdrExposure           .~ 0.5
                        & hdrExposureBias       .~ 0.0
                        & hdrWhitePoint         .~ 11.2
                        & hdrBloomSettings      .~ bloomSettings




camStartPos :: V3 Double
camStartPos = V3 0 0.1 1

mouseSensitivity :: V2 Double
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = fpsCameraMovement camStartPos wasdControlled . fpsCameraRotation mouseControlled

headRotationByInput :: (Real t) => YageWire t a (Quaternion Double)
headRotationByInput =
    let acc         = 20
        att         = 0.87
    in
    smoothRotationByKey acc att ( yAxis ) Key'Right
  . smoothRotationByKey acc att (-yAxis ) Key'Left
  . smoothRotationByKey acc att ( xAxis ) Key'Up
  . smoothRotationByKey acc att (-xAxis ) Key'Down
  . 1

