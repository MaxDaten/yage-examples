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
    { windowSize = (800, 800)
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
main = yageMain "yage-sdf" appConf winSettings (mainWire) sdfRenderSystem (1/60)



sdfRenderSystem :: YageRenderSystem SDFView ()
sdfRenderSystem viewport theView = do
    guiTex <- Pass.runGuiPass (theView^.background) viewport (theView^.gui)
    Pass.screenPass viewport $ fromList [ theView^.background, guiTex ]


mainWire :: (HasTime Double (YageTimedInputState t), Real t) => YageWire t () SDFView
mainWire =
    let bgrColor  = Mat.TexRGB8 `Mat.pxTexture` Mat.thistle
        bgr       = mkTexture2D "Background" bgrColor
        txtColor  = Mat.linearV4 (Mat.opaque Mat.darkseagreen)
        bitmapRes = mkTexture2D "char66" <$> (imageRes $ "res" </> "tex" </> "char" </> "char-66.png")
        sdfRes    = mkTexture2D "char66-50p" <$> (imageRes $ "res" </> "tex" </> "char" </> "char-66-sdf-50perc.png")
    in proc _ -> do
        factor  <- whileKeyDown Key'A . arr (1.0+) . arr sin . arr (*0.5) . time <|> 1 -< ()
        bitmapTex  <- constTextureW bitmapRes -< ()
        sdfTex     <- constTextureW sdfRes -< ()
        let elemSize = factor *^ V2 400 400
        returnA -< SDFView bgr $ emptyGUI & guiElements.at "SDF"  ?~ guiImageSDF sdfTex txtColor 0 elemSize
                                          & guiElements.at "BIT"  ?~ guiImage bitmapTex txtColor (V2 400 0) elemSize
                                          & guiElements.at "SDFP" ?~ guiImage sdfTex    txtColor (V2 0 400) elemSize



instance LinearInterpolatable SDFView where
    lerp alpha u v = u & gui .~ lerp alpha (u^.gui) (v^.gui)
