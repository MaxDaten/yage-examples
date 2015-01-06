{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Examples.Shared
    ( module Yage.Examples.Shared
    ) where

import Yage.Prelude hiding (concatMap)
import Yage.Lens

import Yage.Scene
import Yage.Material
import Yage.Font
import Yage.Geometry3D hiding (Cube)
import Yage.Formats.Ygm

import Yage.Rendering.Pipeline.Deferred

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

vertexFormat :: Pos GLfloat -> Tex GLfloat -> TBN GLfloat -> Vertex (Y'P3TX2TN GLfloat)
vertexFormat = internalFormat

buildMeshUV name pos tex = meshFromTriGeo name $ buildTriGeo vertexFormat pos tex

boxEntity :: (Default mat) => Entity (Mesh (Vertex (Y'P3TX2TN GLfloat))) mat
boxEntity =
    ( basicEntity :: Default mat => Entity (Mesh (Vertex (Y'P3TX2TN GLfloat))) mat )
        & renderData .~ buildMeshUV "box" (cubePos 1) (cubeSingleUV)


floorEntity :: Default mat => Entity (Mesh (Vertex (Y'P3TX2TN GLfloat))) mat
floorEntity =
    ( basicEntity :: Default mat => Entity (Mesh (Vertex (Y'P3TX2TN GLfloat))) mat )
        & renderData .~ buildMeshUV "floor" (gridPos 1 1) (gridUV 1)


skydome :: SkyEntity
skydome =
    ( basicEntity :: SkyEntity )
        & renderData .~ (mkFromVerticesF "SkyDome" $ map (position3 =:) . vertices . triangles $ geoSphere 2 1)
--}

