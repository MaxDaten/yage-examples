{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PackageImports #-}
module Yage.Examples.Shared
    ( module Yage.Examples.Shared
    ) where

import Yage.Prelude hiding (Text)


import Yage.Rendering hiding (P3, P3N3, _renderData, _renderMode)
import Yage.Scene
import Yage.Rendering.Transformation
import Yage.Primitives
import Yage.Resources
import "yage" Yage.Geometry


---------------------------------------------------------------------------------------------------
-- Entity Definitions

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


boxEntity :: SceneEntity P3N3
boxEntity =
    let mesh      = (vertices . triangles $ normalCalculator FacetteNormals $ cube 1) :: [Vertex P3N3]
    in SceneEntity 
        { _renderData     = Right $ makeMesh "box" mesh
        , _textures       = [ TextureDefinition (0, "textures")
                               (TextureFile ("res" </> "Brown_Leather_Texture.png"))
                            ]
        , _renderMode     = Triangles
        , _transformation = idTransformation
        }

floorEntity :: SceneEntity P3N3
floorEntity =
    let mesh        = vertices . toLines $ normalCalculator FacetteNormals $ grid 25 1 :: [Vertex P3N3]
    in SceneEntity 
        { _renderData     = Right $ makeMesh "floor" mesh
        , _textures       = []
        , _renderMode     = Lines
        , _transformation = idTransformation
        }

objEntity :: VertexResource P3N3 -> SceneEntity P3N3
objEntity res =
    SceneEntity
        { _renderData     = Left res
        , _renderMode     = Triangles
        , _textures       = []
        , _transformation = idTransformation
        }

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
