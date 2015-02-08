{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Examples.Shared
  ( skydome
  , planeMesh
  , cubeMesh
  , sphereMesh
  ) where

import Yage.Prelude hiding (concatMap)
import Yage.Lens
import Yage.GL

import Linear
import Yage.Scene
import Yage.Material
import Yage.Font
import qualified Yage.Vertex as V
import Yage.Geometry3D hiding (Cube)
import "yage" Yage.Geometry
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


boxEntity :: (Default mat) => Entity (Mesh (Vertex (Y'P3TX2TN GLfloat))) mat
boxEntity =
    ( basicEntity :: Default mat => Entity (Mesh (Vertex (Y'P3TX2TN GLfloat))) mat )
        & renderData .~ buildMeshUV "box" (cubePos 1) (cubeSingleUV)

--}

planeMesh :: Mesh YGMVertex
planeMesh = buildMeshUV "floor" (gridPos 4 (1 :: Vec2)) (gridUV 4)

cubeMesh :: YageResource (Mesh YGMVertex)
cubeMesh = meshRes $ loadYGM id ( "res" </> "model" </> "Cube.ygm", mkSelection ["face"] )

sphereMesh :: YageResource (Mesh YGMVertex)
sphereMesh = meshRes $ loadYGM id ( "res" </> "model" </> "sphere.ygm", mkSelection [] )

skydome :: Mesh (V.Position Vec3)
skydome = mkFromVerticesF "SkyDome" $ map V.Position . vertices . triangles $ geoSphere 2 1

buildMeshUV :: (Foldable f, HasTriangles t, Epsilon a, Floating a, RealFrac a ) => MeshId -> f (t (Pos a)) -> f (t (Tex a)) -> Mesh YGMVertex
buildMeshUV name pos tex = meshFromTriGeo name $ buildTriGeo ygmFormat pos tex
