{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Yage.Examples.Shared where

import Yage.Prelude hiding (Text)

import           Data.Text.Lazy (Text)


import Yage.Rendering
import Yage.Rendering.Primitives

import           Yage.Font




---------------------------------------------------------------------------------------------------
-- Entity Definitions

boxEntity :: RenderEntity
boxEntity =
    let shader    = undefined -- ShaderResource "res/glsl/3d/baseTex.vert" "res/glsl/3d/baseTex.frag"
        shdef     = undefined -- perspectiveUniformDef
        mesh      = cubeMesh
        attribs   = \m ->
                    [ "in_vert_position" @= m^.mDataVertices^..traverse.vPosition
                    , "in_vert_normal"   @= m^.mDataVertices^..traverse.vNormal
                    --, "in_vert_color"    @= m^.mDataVertices^..traverse.vColor
                    , "in_vert_texture"  @= m^.mDataVertices^..traverse.vTexture
                    ]
        rdef      = RenderDefinition
            { _rdefData     = makeMesh 4711 "cube" mesh attribs
            , _rdefProgram  = undefined -- (shader, shdef)
            , _rdefTextures = [ TextureDefinition (0, "textures")
                               (TextureFile ("res" </> "Brown_Leather_Texture.png"))
                              ]
            , _rdefMode     = Triangles
            }
    in mkRenderEntity rdef


floorEntity :: RenderEntity
floorEntity =
    let shader      = ShaderResource "res/glsl/3d/base.vert" "res/glsl/3d/base.frag"
        shdef       = perspectiveUniformDef
        mesh        = gridMesh $ V2 20 20
        attribs     = \m -> 
                      [ "in_vert_position" @= m^.mDataVertices^..traverse.vPosition
                      , "in_vert_normal"   @= m^.mDataVertices^..traverse.vNormal
                      --, "in_vert_color"    @= m^.mDataVertices^..traverse.vColor
                      ]
        rdef        = RenderDefinition
                        { _rdefData     = makeMesh 0815 "floor" mesh attribs
                        , _rdefProgram  = (shader, shdef)
                        , _rdefTextures = []
                        , _rdefMode     = Lines
                        }
    in mkRenderEntity rdef



textEntity3D :: FontTexture -> Text -> Int -> RenderText
textEntity3D fontTexture text ident =
    let fontShader        = ShaderResource "res/glsl/3d/baseFont.vert" "res/glsl/3d/baseFont.frag"
        fontShaderDef     = perspectiveUniformDef
        attribs           = \m -> 
                            [ "in_vert_position" @= m^.mDataVertices^..traverse.vPosition
                            , "in_vert_color"    @= m^.mDataVertices^..traverse.vColor
                            , "in_vert_texture"  @= m^.mDataVertices^..traverse.vTexture
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


---------------------------------------------------------------------------------------------------
-- Shader Definitions


perspectiveUniformDef :: ShaderDefinition ()
perspectiveUniformDef = return ()


screenSpaceDef :: ShaderDefinition ()
screenSpaceDef = return ()


screenDef :: ShaderDefinition ()
screenDef = return ()