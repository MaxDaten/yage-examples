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

fontPath :: String
fontPath  = fpToString $ "res" </> "font" </> "SourceCodePro-Regular.otf"

fontchars :: String
fontchars = " !\"#$%&'()*+,-./0123456789:;<=>?" ++
            "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_" ++
            "`abcdefghijklmnopqrstuvwxyz{|}~"


main :: IO ()
main = do
    let descr = FontDescriptor (24^.pt, 24^.pt) (512,512)
    lib  <- makeLibrary
    font <- loadFont lib fontPath descr

    let fontAtlas      :: TextureAtlas Char Mat.Pixel8
        fontAtlas      = emptyAtlas $ AtlasSettings (V2 1024 1024) (0 :: Mat.Pixel8) 5
        markup         = FontMarkup 1.0 1.0
        genFont        = generateFontBitmapTexture font markup Monochrome fontchars fontAtlas
        Right fontTex  = downscaleFontTexture 2 . sdfFontTexture 4 <$> genFont

    Mat.writePng "font-out.png" $ fontTex^.fontMap
    yageMain "yage-font-sdf" appConf winSettings (mainWire fontTex) sdfRenderSystem (1/60)


sdfRenderSystem :: YageRenderSystem SDFView ()
sdfRenderSystem viewport theView = do
    guiTex <- Pass.runGuiPass (theView^.background) viewport (theView^.gui)
    Pass.runScreenPass viewport [ theView^.background, guiTex ]


mainWire :: (HasTime Float (YageTimedInputState t), Real t) => FontTexture -> YageWire t () SDFView
mainWire fontTex =
    let bgrColor  = Mat.TexRGB8 `Mat.pxTexture` Mat.thistle
        bgr       = Texture "Background" def $ Mat.Texture2D bgrColor
        txtColor  = Mat.linearV4 (Mat.opaque Mat.darkseagreen)
        txtBuffer = emptyTextBuffer fontTex
                        & charColor .~ txtColor
                        & buffText  .~ ">>= \\x -> Hallo RealWorld!"

        imgTex   = Mat.mkTextureImg Mat.TexY8 $ fontTex^.fontMap
        tex      = mkTexture (fontTex^.font.to fontName.packedChars) $ Texture2D imgTex


    in proc _ -> do
        returnA -< SDFView
            { _background = bgr
            , _gui        = emptyGUI & guiElements.at "Hallo"       ?~ GUIFont txtBuffer (idTransformation & transPosition._xy .~ V2 50 50
                                                                                                           & transScale._xy .~ V2 2 2)
                                     -- & guiElements.at "FontTexture" ?~ guiImage tex txtColor (V2 0 0) (V2 800 800)
            }



instance LinearInterpolatable SDFView where
    lerp alpha u v = u & gui .~ lerp alpha (u^.gui) (v^.gui)

instance HasResources GeoVertex SDFView SDFView where
    requestResources = return
