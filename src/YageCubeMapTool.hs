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

import           Yage.Camera
import           Yage.Formats.Font
import           Yage.HDR
import           Yage.Scene
import           Yage.Texture
import           Yage.Viewport
import           Yage.Uniforms             as Uniforms
import           Yage.Texture.TextureAtlas

import           Yage.UI.GUI
import           Yage.TH.Shader as GLSL


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
    { _cubeMapMode :: CubeMapMode
    } deriving ( Show, Ord, Eq )

makeLenses ''SceneSettings

type SceneEntity      = Entity (Mesh GeoVertex) ()
type SceneEnvironment = Environment Light SkyEntity
type CubeMapScene     = Scene HDRCamera SceneEntity SceneEnvironment GUI


main :: IO ()
main = yageMain "yage-material" appConf winSettings mainWire cubemapPipeline (1/60)


mainWire :: (HasTime Double (YageTimedInputState t), RealFrac t, Show t) => YageWire t () (CubeMapScene, SceneSettings)
mainWire = proc () -> do
    cam <- hdrCameraHandle `overA` cameraControl -< camera
    sky <- skyDomeW -< cam^.hdrCameraHandle.cameraLocation

    gui <- guiWire -< ()

    dummy <- {--(entityOrientation `overA` previewRotationByInput) .--} dummyEntityW modelRes -< ()

    cubeMode <- toggle (keyJustPressed Key'N) SurfaceNormal ViewReflection -< ()

    let settings = SceneSettings cubeMode
        scene    :: CubeMapScene
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

    modelRes = meshResource $ loadYGM geoVertex $ ("res"</>"model"</>"sphere"<.>"ygm", mempty)

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

    skyDomeW :: YageWire t (V3 Double) SkyEntity
    skyDomeW = proc pos -> do
        let cubeTex = cubeTextureToTexture "SkyCube" $ mkTexture2D "" <$>
             Cube  { cubeFaceRight  = Mat.TexRGB8 `Mat.pxTexture` Mat.red
                   , cubeFaceLeft   = Mat.TexRGB8 `Mat.pxTexture` Mat.green
                   , cubeFaceTop    = Mat.TexRGB8 `Mat.pxTexture` Mat.blue
                   , cubeFaceBottom = Mat.TexRGB8 `Mat.pxTexture` Mat.cyan
                   , cubeFaceFront  = Mat.TexRGB8 `Mat.pxTexture` Mat.magenta
                   , cubeFaceBack   = Mat.TexRGB8 `Mat.pxTexture` Mat.yellow
                   }
        -- tex <- cubeTexture . pure <$> constTextureW skyTex -< ()
        returnA -< skydome & materials.Mat.matTexture .~ cubeTex
                           & entityPosition           .~ pos
                           & entityScale              .~ 50

    skyTex  = textureResource $ texDir</>"misc"</>"blueprint"</>"Seamless Blueprint Textures"</>"1"<.>"png"

    guiWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () GUI
    guiWire = pure emptyGUI


-- Camera Control Wires
camStartPos :: V3 Double
camStartPos = V3 0 0 1.5

mouseSensitivity :: V2 Double
mouseSensitivity = V2 0.1 0.1

wasdControlled :: Real t => YageWire t () (V3 Double)
wasdControlled = wasdMovement (V2 2 2)

mouseControlled :: Real t => YageWire t () (V2 Double)
mouseControlled = whileKeyDown Key'LeftControl . arr (mouseSensitivity *) . mouseVelocity <|> 0

cameraControl :: Real t => YageWire t Camera Camera
cameraControl = fpsCameraMovement camStartPos wasdControlled . fpsCameraRotation mouseControlled


-- Dummy Control Wires
-- dummyControl :: Real t => YageWire t Dummy Dummy
-- dummyControl = overA transOrientation dummyRotationByInput

previewRotationByInput :: (Real t) => YageWire t (Quaternion Double) (Quaternion Double)
previewRotationByInput =
    let acc         = 20
        att         = 0.87
    in
   smoothRotationByKey acc att ( yAxis ) Key'Right
 . smoothRotationByKey acc att (-yAxis ) Key'Left
 . smoothRotationByKey acc att ( xAxis ) Key'Up
 . smoothRotationByKey acc att (-xAxis ) Key'Down

-------------------------------------------------------------------------------
-- Render Pass Definition


cubemapPipeline :: Viewport Int -> (CubeMapScene, SceneSettings) -> RenderSystem ()
cubemapPipeline viewport (scene, settings) =
    let cam                     = scene^.sceneCamera.hdrCameraHandle
        baseDescr               = simpleCubeMapped viewport
        runBasePass             = runRenderPass baseDescr
        envMap                  = (scene^.sceneEnvironment.envSky^?!_Just)^.materials
        baseData                :: ShaderData SceneFrameUni '[ TextureSampler "EnvironmentCubeMap" ]
        baseData                = ShaderData ( perspectiveUniforms (fromIntegral <$> viewport) cam ) RNil
                                    & shaderUniforms <<+>~ ( SField =: (fromIntegral . fromEnum $ settings^.cubeMapMode) )
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

type SceneEntityUni = [ YModelMatrix, YNormalMatrix ]
type SceneFrameUni  = PerspectiveUniforms ++ '["SurfaceNormal" ::: V1 GL.GLint]
type CubeMappedShader = Shader (SceneFrameUni ++ SceneEntityUni) '[ YMaterialTex "EnvironmentCubeMap" ] GeoVertex
type CubeMappedPass = PassDescr CubeMappedChannels CubeMappedShader


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
baseVertexProgram = [GLSL.yVertex|
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
baseFragmentProgram = [GLSL.yFragment|
#version 410 core

#define SURFACE_NORMAL 0
#define VIEW_REFLECTION 1

uniform mat4        ViewMatrix;
uniform samplerCube EnvironmentCubeMap;
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

    OutColor.rgb = texture( EnvironmentCubeMap, dir ).rgb;
    OutColor.a   = 1.0;
}
|]
