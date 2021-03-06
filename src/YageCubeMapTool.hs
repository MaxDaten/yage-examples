{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main where
import           Yage                      hiding ( until )
import           Yage.Lens                 hiding ( (<.>) )
import           Yage.Math                 hiding ( lerp )
import           Yage.Wire                 hiding ( at, (<>), (<+>) )

import           Data.List                 ( cycle )

import           Yage.Camera
import           Yage.Formats.Font
import           Yage.HDR
import           Yage.Scene
import           Yage.Texture
import           Yage.Viewport
import           Yage.Uniform             as Uniforms
import           Yage.Texture.TextureAtlas

import           Yage.UI.GUI
import           Yage.TH.Shader as GLSL

import           Yage.Formats.AMDCubeMap


import           Yage.Examples.Shared
import qualified Yage.Material             as Mat
import           Yage.Pipeline.Deferred    hiding ( toRenderEntity )
import qualified Yage.Resources            as Res
import           Yage.Transformation
import qualified Yage.Core.OpenGL          as GL

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


appConf :: ApplicationConfig
appConf = defaultAppConfig{ logPriority = WARNING }

-------------------------------------------------------------------------------
-- View Definition
data CubeMapMode = SurfaceNormal | ViewReflection
    deriving ( Show, Ord, Eq, Enum )

data SceneSettings = SceneSettings
    { _cubeMapMode      :: CubeMapMode
    , _cubeMipMapLevel  :: Float
    } deriving ( Show, Ord, Eq )

makeLenses ''SceneSettings

type SceneEntity      = Entity (Mesh GeoVertex) ()
type SceneEnvironment = Environment Light SkyEntity
type CubeMapScene     = Scene HDRCamera SceneEntity SceneEnvironment GUI


main :: IO ()
main = yageMain "yage-material" appConf winSettings mainWire cubemapPipeline (1/60)


mainWire :: (HasTime Double (YageTimedInputState t), RealFrac t) => YageWire t () (CubeMapScene, SceneSettings)
mainWire = proc () -> do
    cam <- hdrCameraHandle `overA` cameraControl -< camera
    sky <- skyDomeW -< cam^.hdrCameraHandle.cameraLocation

    gui <- guiWire -< ()

    dummy <- {--(entityOrientation `overA` previewRotationByInput) .--} dummyEntityW modelRes -< ()

    settings <- settingsControl -< ()

    let scene    :: CubeMapScene
        scene    = emptyScene cam gui
                    & sceneSky      ?~ sky
                    & sceneEntities .~ fromList [ dummy ]
                    & sceneLights   .~ fromList [ mainLight, specLight ]
    returnA -< ( scene, settings )


    where
    texDir :: FilePath
    texDir = "res"</>"tex"

    bloomSettings   = defaultBloomSettings
                        & bloomFactor           .~ 0.7
                        & bloomPreDownsampling  .~ 2
                        & bloomGaussPasses      .~ 5
                        & bloomWidth            .~ 2
                        & bloomThreshold        .~ 0.5

    camera          = defaultHDRCamera ( mkCameraFps (deg2rad 75) (0.1,10000) )
                        & hdrExposure           .~ 2
                        & hdrExposureBias       .~ 0.0
                        & hdrWhitePoint         .~ 11.2
                        & hdrBloomSettings      .~ bloomSettings

    modelRes = meshRes $ loadYGM geoVertex $ ("res"</>"model"</>"sphere"<.>"ygm", mempty)

    dummyEntityW :: YageResource (Mesh GeoVertex) -> YageWire t () SceneEntity
    dummyEntityW meshRes = proc () -> do
        entity <- renderData <~~ constMeshW meshRes -< boxEntity :: SceneEntity
        returnA -< entity & entityScale //~ 2.0

    mainLight  = Light
                { _lightType      = Pointlight (V3 15 1 15) 100
                , _lightColor     = V3 1.0 1.0 1.0
                , _lightIntensity = 0.5
                }

    specLight  = Light
                { _lightType      = Pointlight (V3 2 1 2) 10
                , _lightColor     = V3 1.0 1.0 1.0
                , _lightIntensity = 1
                }


    skyDomeW :: Real t => YageWire t (V3 Double) SkyEntity
    skyDomeW = proc pos -> do
        (initEvent, toggleEvent) <- now &&& keyJustPressed Key'T -< ()
        selectedTex <- hold . allocationOnEvent . popOnEvent cubemapCycle -< initEvent `mergeL` (() <$ toggleEvent)
        returnA -< skydome & materials.skyEnvironmentMap
                                      .Mat.matTexture .~ selectedTex
                           & entityPosition           .~ pos
                           & entityScale              .~ 50

    -- mkCubeMapRes ident dir =
    --     let selection = amdSeperateFiles dir "png"
    --     in (mkTextureCubeMip ident <$> singleCubemapMipFiles selection)
    --             <&> textureConfig.texConfWrapping.texWrapClamping .~ GL.ClampToEdge

    cubemapCycle = cycle [pure defaultCubeMap, skyTex, graceCrossTex]
    skyTex        = mkTextureCubeMip "SeaCross" <$>
                        cubeCrossMipsRes Strip (texDir</>"env"</>"Sea"</>"pmrem"</>"sea_m<->.png")
                            <&> textureConfig.texConfWrapping.texWrapClamping .~ GL.ClampToEdge
    graceCrossTex = mkTextureCubeMip "GraceCross" <$>
                        cubeCrossMipsRes Strip (texDir</>"env"</>"grace"</>"pmrem"</>"grace_m<->.png")
                            <&> textureConfig.texConfWrapping.texWrapClamping .~ GL.ClampToEdge
    guiWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () GUI
    guiWire = pure emptyGUI


-- Camera Control Wires
camStartPos :: V3 Double
camStartPos = V3 0 0 1.5

mouseSensitivity :: V2 Double
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: (Real t) => YageWire t () (V2 Double)
mouseControlled = (whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity) <|> 0

cameraControl :: (Real t) => YageWire t Camera Camera
cameraControl = arcBallRotation mouseControlled . arr (0,) . fpsCameraMovement camStartPos wasdControlled


settingsControl :: (Real t) => YageWire t () SceneSettings
settingsControl =
    SceneSettings <$> toggle (keyJustPressed Key'N) SurfaceNormal ViewReflection
                  <*> ( spin (0, 10) 0 <<< (( 1 <$) <$> keyJustPressed Key'Period) &&&
                                           ((-1 <$) <$> keyJustPressed Key'Comma) )

-- Dummy Control Wires
-- dummyControl :: Real t => YageWire t Dummy Dummy
-- dummyControl = overA transOrientation dummyRotationByInput

-- previewRotationByInput :: (Real t) => YageWire t (Quaternion Double) (Quaternion Double)
-- previewRotationByInput =
--     let acc         = 20
--         att         = 0.87
--     in
--    smoothRotationByKey acc att ( yAxis ) Key'Right
--  . smoothRotationByKey acc att (-yAxis ) Key'Left
--  . smoothRotationByKey acc att ( xAxis ) Key'Up
--  . smoothRotationByKey acc att (-xAxis ) Key'Down

-------------------------------------------------------------------------------
-- Render Pass Definition

-- @TODO http://the-witness.net/news/2012/02/seamless-cube-map-filtering/

cubemapPipeline :: Viewport Int -> (CubeMapScene, SceneSettings) -> RenderSystem ()
cubemapPipeline viewport (scene, settings) =
    let cam                     = scene^.sceneCamera.hdrCameraHandle
        baseDescr               = simpleCubeMapped viewport
        runBasePass             = runRenderPass baseDescr
        envMap                  = (scene^.sceneEnvironment.envSky^?!_Just)^.materials.skyEnvironmentMap
        baseData                :: ShaderData SceneFrameUni '[ TextureSampler "EnvironmentCubeMap" ]
        baseData                = ShaderData ( perspectiveUniforms (fromIntegral <$> viewport) cam ) RNil
                                    & shaderUniforms <<+>~ ( SField =: (fromIntegral . fromEnum $ settings^.cubeMapMode) )
                                    & shaderUniforms <<+>~ ( textureLod     =: (realToFrac $ settings^.cubeMipMapLevel))
                                    & shaderTextures <<+>~ ( textureSampler =: (envMap^.Mat.matTexture) )
        baseTex                 = baseDescr^.renderTargets.to baseColorChannel

        skyData                 = skyFrameData viewport cam
        skyChannels             = RenderTarget "fbo-sky" SkyInChannels
                                    { sBufferChannel = baseDescr^.renderTargets.to baseColorChannel
                                    , sDepthChannel  = baseDescr^.renderTargets.to baseDepthChannel
                                    }
        runSkyPass              = runRenderPass $ skyPass skyChannels viewport
    in do
    baseData `runBasePass` ( toSceneEntity cam <$> scene^.sceneEntities )
    skyData  `runSkyPass`  ( toSkyEntity <$> scene^.sceneEnvironment.envSky.to repack )

    guiTex <- runGuiPass baseTex viewport ( scene^.sceneGui )

    screenPass viewport [ baseTex, guiTex ]


data CubeMappedChannels = CubeMappedChannels
    { baseColorChannel :: Texture
    , baseDepthChannel :: Texture
    }

type SceneEntityUni   = [ YModelMatrix, YNormalMatrix ]
type SceneFrameUni    = PerspectiveUniforms ++ '["SurfaceNormal" ::: V1 GL.GLint, TextureLod "MipMapLevel"]
type CubeMappedShader = Shader (SceneFrameUni ++ SceneEntityUni) '[ YMaterialTex "EnvironmentCubeMap" ] GeoVertex
type CubeMappedPass   = PassDescr CubeMappedChannels CubeMappedShader


simpleCubeMapped :: Viewport Int -> CubeMappedPass
simpleCubeMapped viewport =
    let thePass     = passPreset target (viewport^.rectangle) (ShaderUnit shaderProg)
        clearBuffer = (thePass^.passPreRendering) >> io (GL.clear [ GL.DepthBuffer ])
    in thePass & passPreRendering .~ clearBuffer

    where

    shaderProg = ShaderProgramUnit
                 { _shaderName       = "YageCubeMapTool.hs"
                 , _shaderSources    = [ baseVertexProgram^.shaderSource
                                       , baseFragmentProgram^.shaderSource
                                       ]
                 }

    target  = RenderTarget "cubed-fbo" $ CubeMappedChannels
                { baseColorChannel = mkTargetTexture "base-color" baseSpec
                , baseDepthChannel = mkTargetTexture "base-depth"  depthSpec
                }

    baseSpec   = Mat.mkTextureSpec' (viewport^.rectangle.extend) GL.RGBA
    depthSpec  = Mat.mkTextureSpec (viewport^.rectangle.extend) GL.UnsignedByte GL.DepthComponent GL.DepthComponent24



toSceneEntity :: Camera -> SceneEntity -> RenderEntity GeoVertex (ShaderData SceneEntityUni '[])
toSceneEntity camera ent = toRenderEntity shaderData ent
    where
    shaderData = ShaderData uniforms RNil
    uniforms =
        modelMatrix       =: ( ent^.entityTransformation.transformationMatrix & traverse.traverse %~ realToFrac )           <+>
        normalMatrix      =: ( theNormalMatrix & traverse.traverse %~ realToFrac )

    theNormalMatrix :: M33 Double
    theNormalMatrix =
        let invCam        = camera & cameraTransformation %~ inverseTransformation
            invViewM      = fmap realToFrac <$> invCam^.cameraMatrix
            invModelM     = ent^.entityTransformation.to inverseTransformation.transformationMatrix
        in adjoint $ invModelM^.to m44_to_m33 !*! invViewM^.to m44_to_m33


instance FramebufferSpec CubeMappedChannels RenderTargets where
    fboColors CubeMappedChannels{baseColorChannel} =
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D baseColorChannel 0
        ]

    fboDepth CubeMappedChannels{baseDepthChannel} =
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D baseDepthChannel 0

instance LinearInterpolatable (CubeMapScene, SceneSettings) where
    lerp alpha u v = (lerp alpha (u^._1) (v^._1), v^._2)

-------------------------------------------------------------------------------
-- | Vertex code
baseVertexProgram :: GLSL.ShaderSource VertexShader
baseVertexProgram = [yVertex|
#version 410 core

uniform mat4 ViewMatrix;
uniform mat4 VPMatrix;
uniform mat4 ModelMatrix;
uniform mat3 NormalMatrix;

// naturally in model-space
in vec3 vPosition;
in vec2 vTexture;
in vec3 vTangentX;
in vec4 vTangentZ;

out mat3 TangentToView;
out vec3 positionView;

void main()
{
    mat4 ModelToView     = ViewMatrix * ModelMatrix;
    mat4 ModelToProj     = VPMatrix * ModelMatrix;


    vec3 tangentZ        = normalize( NormalMatrix * vTangentZ.xyz );
    vec3 tangentX        = normalize( NormalMatrix * vTangentX.xyz );
    vec3 tangentY        = normalize( cross( tangentZ, tangentX ) * vTangentZ.w );
    TangentToView        = mat3( tangentX, tangentY, tangentZ );

    positionView    = vec3( ModelToView * vec4( vPosition, 1.0 ) );
    gl_Position     = ModelToProj * vec4( vPosition, 1.0 );
}
|]

-------------------------------------------------------------------------------
-- | Fragment code
baseFragmentProgram :: GLSL.ShaderSource FragmentShader
baseFragmentProgram = [yFragment|
#version 410 core

#define SURFACE_NORMAL 0
#define VIEW_REFLECTION 1

uniform mat4        ViewMatrix;
uniform samplerCube EnvironmentCubeMap;
uniform float       MipMapLevel;
uniform int         SurfaceNormal;
uniform vec3        eyeIn;

smooth in mat3 TangentToView;
smooth in vec3 positionView;

// Red Green Blue Depth
layout (location = 0) out vec4 OutColor;

void main()
{
    mat4 ViewToWorld = inverse( ViewMatrix );
    vec3 normal  = normalize (TangentToView * vec3(0, 0, 1) );

    vec3 dir;
    if ( SurfaceNormal == SURFACE_NORMAL )
    {
        dir = vec3( ViewToWorld * vec4( normal, 0.0 ) );;
    }
    else
    {
        vec3 eyeIn = normalize( positionView );
        dir = vec3( ViewToWorld * vec4(reflect( eyeIn, normal ), 0.0) );
    }

    OutColor.rgb = textureLod( EnvironmentCubeMap, dir, MipMapLevel ).rgb;
    OutColor.a   = 1.0;
}
|]
