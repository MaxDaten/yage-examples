{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Examples.Shared
    ( module Yage.Examples.Shared
    ) where

import Yage.Prelude hiding (concatMap)
import Yage.Lens

import Yage.Rendering hiding (P3, P3N3, P3T2, P3TX2NT3, _renderData, _drawSettings)
import Yage.Scene
import Yage.Material
import Yage.Transformation
import "yage" Yage.Geometry
import Yage.Geometry3D hiding (Cube)
import qualified Yage.Geometry3D as Geo ( Cube )
import Yage.Formats.Ygm

import Data.List (concatMap)

import Yage.Pipeline.Deferred

---------------------------------------------------------------------------------------------------
-- Entity Definitions

{--
sphereEntity :: Int -> SceneEntity P3N3
sphereEntity subdivides =
    let mesh      = (vertices . triangles $ normalCalculator SphericalNormals $ geoSphere subdivides 0.5) :: [Vertex P3N3]            
    in SceneEntity 
        { _renderData     = Right $ makeMesh "sphere" mesh
        , _textures       = []
        , _renderMode     = Triangles
        , _transformation = idTransformation
        }

coneEntity :: Int -> SceneEntity P3N3
coneEntity divs =
    let mesh      = (vertices . triangles $ normalCalculator SphericalNormals $ cone 0.5 1 divs) :: [Vertex P3N3]
    in SceneEntity 
        { _renderData     = Right $ makeMesh "cone" mesh
        , _textures       = []
        , _renderMode     = Triangles
        , _transformation = idTransformation
        }

pyramidEntity :: SceneEntity P3N3
pyramidEntity =
    let mesh      = (vertices . triangles $ normalCalculator FacetteNormals $ pyramid 1) :: [Vertex P3N3]
    in SceneEntity 
        { _renderData     = Right $ makeMesh "pyramid" mesh
        , _textures       = []
        , _renderMode     = Triangles
        , _transformation = idTransformation
        }

--}
vertexFormat :: Pos GLfloat -> Tex GLfloat -> TBN GLfloat -> Vertex (Y'P3TX2TN GLfloat)
vertexFormat = internalFormat

buildMesh' :: (Storable (Vertex vert)) => Text -> Primitive (Vertex vert) -> Mesh (Vertex vert)
buildMesh' name = mkFromVerticesF name . vertices . triangles

buildMeshUV name pos tex = meshFromTriGeo name $ buildTriGeo vertexFormat pos tex

boxEntity :: (Default mat) => Entity (MeshResource (Vertex (Y'P3TX2TN GLfloat))) mat
boxEntity = 
    let cubeMesh = buildMeshUV "box" (cubePos 1) (cubeSingleUV) 
    in ( basicEntity :: Default mat => Entity (MeshResource (Vertex (Y'P3TX2TN GLfloat))) mat )
            & renderData .~ MeshPure cubeMesh


floorEntity :: Entity (MeshResource (Vertex (Y'P3TX2TN GLfloat))) (ResourceMaterial)
floorEntity =
    let gridMesh = buildMeshUV "floor" (gridPos 25 1) (gridUV 25)
    in ( basicEntity :: Entity (MeshResource (Vertex (Y'P3TX2TN GLfloat))) ResourceMaterial )
            & renderData .~ ( MeshPure gridMesh )


skydome :: Material (Cube TextureResource) -> SkyEntityRes
skydome cubeTex = 
    ( basicEntity :: Entity (MeshResource (Vertex (Y'P3 GLfloat))) (AResourceMaterial Cube) ) -- we have to fix the functor type
        & materials  .~ cubeTex
        & renderData .~ (buildMesh' "SkyDome" $ geoSphere 2 1)

{--
--}

{--
textEntity3D :: FontTexture -> Text -> Int -> RenderText
textEntity3D fontTexture text ident =
    let fontShader        = ShaderResource "res/glsl/3d/baseFont.vert" "res/glsl/3d/baseFont.frag"
        fontShaderDef     = perspectiveUniformDef
        attribs           = [ {-- "in_vert_position" @= m^.mDataVertices^..traverse.vPosition
                            , "in_vert_color"    @= m^.mDataVertices^..traverse.vColor
                            , "in_vert_texture"  @= m^.mDataVertices^..traverse.vTexture --}
                            ]
    in RenderText 
        { _textIdent   = ident
        , _textBuffer  = emptyTextBuffer fontTexture `writeText` text
        , _textTexCh   = (0, "textures") 
        , _textShader  = (fontShader, fontShaderDef)
        , _textAttribs = attribs
        , _textTransf  = idTransformation
        }

textEntity2D :: FontTexture -> Text -> Int -> RenderText
textEntity2D fontTexture text ident = 
    (textEntity3D fontTexture text ident) 
    & textShader.shaderDef .~ screenSpaceDef
    & textShader.shaderRes .~ ShaderResource "res/glsl/3d/baseFont.vert" "res/glsl/3d/baseFont.frag"

--}

---------------------------------------------------------------------------------------------------
-- Shader Definitions
