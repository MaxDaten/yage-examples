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
import Yage.Formats.Font

import Yage.Texture.Atlas.Builder
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
    { windowSize = (1000, 230)
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

fontPath :: FilePath
--fontPath  = "res" </> "font" </> "yft" </> "SourceCodePro-Regular512.yft"
fontPath  = "res" </> "font" </> "yft" </> "SourceCodePro-Regular1024.yft"
--fontPath  = "res" </> "font" </> "yft" </> "SourceCodePro-Regular4096.yft"

--fontPath  = "res" </> "font" </> "yft" </> "SourceSansPro-Regular512.yft"
--fontPath  = "res" </> "font" </> "yft" </> "SourceSansPro-Regular1024.yft"
--fontPath  = "res" </> "font" </> "yft" </> "SourceSansPro-Regular4096.yft"


main :: IO ()
main = do
    fontTexture <- readFontTexture fontPath
    yageMain "yage-font-sdf" appConf winSettings (mainWire fontTexture) sdfRenderSystem (1/60)


sdfRenderSystem :: YageRenderSystem SDFView ()
sdfRenderSystem viewport theView = do
    guiTex <- Pass.runGuiPass (theView^.background) viewport (theView^.gui)
    Pass.runScreenPass viewport [ theView^.background, guiTex ]


mainWire :: (HasTime Float (YageTimedInputState t), Real t) => FontTexture -> YageWire t () SDFView
mainWire fontTex =
    let bgrColor  = Mat.TexRGB8 `Mat.pxTexture` Mat.sRGB24 66 85 114
        bgr       = Texture "Background" def $ Mat.Texture2D bgrColor

        txtColor  = Mat.sRGBV4 $ Mat.opaque $ Mat.sRGB24 253 96 65
        txtBuffer = emptyTextBuffer fontTex
                        & charColor .~ txtColor
                        & buffText  .~ "A monad is just a monoid \nin the category of endofunctors"

        imgTex   = Mat.mkTextureImg Mat.TexY8 $ fontTex^.fontMap
        tex      = mkTexture ( fontTex^.fontMetric.fontName.packedChars ) $ Texture2D imgTex


    in proc _ -> do
        scale <- integrateBounded (1,10) 1 . ( whileKeyDown Key'Up . 1 <|> -1 ) -< ()
        returnA -< SDFView
            { _background = bgr
            , _gui        = emptyGUI & guiElements.at "Hallo"       ?~ GUIFont txtBuffer (idTransformation & transPosition._xy .~ V2 50 180
                                                                                                           & transScale._xy .~ scale *^ V2 3 3)
                                     -- & guiElements.at "FontTexture" ?~ guiImage tex txtColor (V2 0 0) (V2 800 800)
            }



instance LinearInterpolatable SDFView where
    lerp alpha u v = u & gui .~ lerp alpha (u^.gui) (v^.gui)

instance HasResources GeoVertex SDFView SDFView where
    requestResources = return
