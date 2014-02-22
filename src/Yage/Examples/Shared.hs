{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Yage.Examples.Shared
    ( module Yage.Examples.Shared
    , module Prims
    ) where

import Yage.Prelude hiding (Text)

import Yage.Rendering hiding (P3, P3N3)
import Yage.Primitives
import Yage.Primitives as Prims (NormalSmoothness(..))

import           Yage.Vertex




---------------------------------------------------------------------------------------------------
-- Entity Definitions

sphereEntity :: Int -> RenderEntity P3N3
sphereEntity subdivides =
    let mesh      = (vertices . triangles $ normalCalculator SphericalNormals $ geoSphere subdivides 0.5) :: [Vertex P3N3]
        rdef      = RenderDefinition
            { _rdefData     = makeMesh "sphere" mesh
            , _rdefTextures = []
            , _rdefMode     = Triangles
            }
    in mkRenderEntity rdef


coneEntity :: Int -> RenderEntity P3N3
coneEntity divs =
    let mesh      = (vertices . triangles $ normalCalculator SphericalNormals $ cone 0.5 1 divs) :: [Vertex P3N3]
        rdef      = RenderDefinition
            { _rdefData     = makeMesh "cone" mesh
            , _rdefTextures = []
            , _rdefMode     = Triangles
            }
    in mkRenderEntity rdef

pyramidEntity :: RenderEntity P3N3
pyramidEntity =
    let mesh      = (vertices . triangles $ normalCalculator FacetteNormals $ pyramid 1) :: [Vertex P3N3]
        rdef      = RenderDefinition
            { _rdefData     = makeMesh "pyramid" mesh
            , _rdefTextures = []
            , _rdefMode     = Triangles
            }
    in mkRenderEntity rdef


boxEntity :: RenderEntity P3N3
boxEntity =
    let mesh      = (vertices . triangles $ normalCalculator FacetteNormals $ cube 1) :: [Vertex P3N3]
        rdef      = RenderDefinition
            { _rdefData     = makeMesh "cube" mesh
            , _rdefTextures = [ TextureDefinition (0, "textures")
                               (TextureFile ("res" </> "Brown_Leather_Texture.png"))
                              ]
            , _rdefMode     = Triangles
            }
    in mkRenderEntity rdef


floorEntity :: RenderEntity P3N3
floorEntity =
    let mesh        = vertices . toLines $ normalCalculator FacetteNormals $ grid 100 1 :: [Vertex P3N3]
        rdef        = RenderDefinition
                        { _rdefData     = makeMesh "floor" mesh
                        , _rdefTextures = []
                        , _rdefMode     = Lines
                        }
    in mkRenderEntity rdef


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
