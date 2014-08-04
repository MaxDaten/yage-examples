{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Arrows #-}

module Main where

import Yage
import Yage.Math hiding (lerp)
import Yage.Lens hiding ((<.>))
import Yage.Wire hiding ((<>), (<+>), at)

import Yage.Camera
import Yage.Scene
import Yage.HDR
import Yage.Texture
import "yage" Yage.Geometry
import Yage.Rendering.Mesh

import Yage.UI.GUI
import qualified Yage.Core.OpenGL as GL

import Yage.Transformation
import qualified Yage.Resources as Res
import qualified Yage.Material  as Mat
import Yage.Pipeline.Deferred
import Yage.Examples.Shared

import Yage.Pipeline.Deferred.ScreenPass      as Pass
import Yage.Pipeline.Deferred.GuiPass         as Pass


winSettings :: WindowConfig
winSettings = WindowConfig
    { windowSize = (1200, 800)
    , windowHints =
        [ WindowHint'ContextVersionMajor  4
        , WindowHint'ContextVersionMinor  1
        , WindowHint'OpenGLProfile        OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat  True
        , WindowHint'RefreshRate          60
        ]
     }


appConf :: ApplicationConfig
appConf = defaultAppConfig{ logPriority = WARNING }

data SDFView = SDFView
    { _background     :: Texture
    , _gui            :: GUI
    }

makeLenses ''SDFView


main :: IO ()
main = do
    bitTexture <- loadTexture2D $ "res" </> "tex" </> "char" </> "char-66.png"
    sdfTexture <- loadTexture2D $ "res" </> "tex" </> "char" </> "char-66-sdf.png"
    --sdfTexture <- loadTexture2D "/Users/jloos/Desktop/Bildschirmfoto-2014-08-04-um-11.05.01.png"
    --sdfTexture <- loadTexture2D "/Users/jloos/Dropbox/Screenshots/Yage/Bildschirmfoto 2014-08-04 um 11.05.01.png"
    yageMain "yage-sdf" appConf winSettings (mainWire bitTexture sdfTexture) sdfRenderSystem (1/60)



sdfRenderSystem :: YageRenderSystem SDFView ()
sdfRenderSystem viewport theView = do
    guiTex <- Pass.runGuiPass (theView^.background) viewport (theView^.gui)
    Pass.runScreenPass viewport [ theView^.background, guiTex ]


mainWire :: (HasTime Float (YageTimedInputState t), Real t) => Texture -> Texture -> YageWire t () SDFView
mainWire bitmap sdf =
    let bgrColor = Mat.TexRGB8 `Mat.pxTexture` Mat.thistle
        bgr      = Texture "Background" def $ Mat.Texture2D bgrColor
        txtColor = Mat.linearV4 (Mat.opaque Mat.darkseagreen)

    in proc _ -> do
        factor <- arr ((+) 1.5) . arr sin . time -< ()
        returnA -< SDFView bgr $ emptyGUI & guiElements.at "SDF" ?~ guiImageSDF sdf txtColor 0 (V2 (400*factor) (400*factor))
                                          & guiElements.at "BIT" ?~ guiImage bitmap txtColor (V2 450 0) (V2 (400*factor) (400*factor))



instance LinearInterpolatable SDFView where
    lerp alpha u v = u & gui .~ lerp alpha (u^.gui) (v^.gui)

instance HasResources GeoVertex SDFView SDFView where
    requestResources = return
