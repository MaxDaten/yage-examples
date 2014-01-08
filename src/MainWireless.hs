{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Yage.Prelude hiding (Text)

import           Data.List
import           Data.Typeable


import           Linear
import           Yage.Math
import           Yage.Font
import           Yage.UI
import qualified Yage.Text as T

import           Data.Text.Lazy (Text)
import           Yage.Rendering
import           Yage.Rendering.Texture
import           Yage.Texture.Atlas


import           Yage.Core.Application
import           Yage.Core.Application.Logging
import           Yage.Core.Application.Loops



import           Yage.Examples.Shared

hints :: [WindowHint]
hints = [ WindowHint'ContextVersionMajor  3
        , WindowHint'ContextVersionMinor  2
        , WindowHint'OpenGLProfile        OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat  True
        , WindowHint'RefreshRate          60
        --, WindowHint'Resizable            False
        --, WindowHint'Decorated            False
        ]

fontchars :: String
fontchars = " !\"#$%&'()*+,-./0123456789:;<=>?" ++
            "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_" ++
            "`abcdefghijklmnopqrstuvwxyz{|}~"
fontPath :: String
fontPath  = encodeString $ "res" </> "font" </> "SourceCodePro-Regular.otf"

fontAtlas :: TextureAtlas i Pixel8
fontAtlas = emptyAtlas 1024 1024 (0 :: Pixel8) 5

class (Typeable i, Typeable a) => IsUpdateable i a where
    update :: i -> a -> a
    toUpdateable :: (Typeable u) => u -> Maybe a
    toUpdateable = cast


newtype Box = Box RenderEntity
    deriving (Typeable, Renderable)

newtype ReactingText = ReactingText RenderText
    deriving (Typeable, Renderable)

instance IsUpdateable InputState ReactingText where
    update input (ReactingText rt) =
        let mousePos = input^.mouse.mousePosition
        in ReactingText $ rt & textBuffer %~ (`setText` (T.format "({},{})" mousePos))

instance IsUpdateable InputState Box where
    update input (Box ent@RenderEntity{..}) =
        let rotV k a d = axisAngle a $ deg2rad (if input `isPressed` k then d else 0.0)
            rot        =  rotV Key'Right zAxis (-1.0)
                        * rotV Key'Left  zAxis   1.0
                        * rotV Key'Down  xAxis   1.0
                        * rotV Key'Up    xAxis (-1.0)
        in  Box $ ent &  entityOrientation .~ signorm (ent^.entityOrientation * rot)


tryWithSomeRenderable :: (Typeable u, Renderable r) => (u -> r) -> SomeRenderable -> SomeRenderable
tryWithSomeRenderable f some = maybe some (toRenderable . f) (fromRenderable some)


hellWorld :: Text
hellWorld = "Hallo Welt! :)\nZeilenumbruch"
main :: IO ()
main =
    let scene        = testScene
        gui          = emptyRenderScene $ Camera2D fpsCamera
        conf         = defaultAppConfig{ logPriority = WARNING }
        size         = (800,600)
        factor       = 2
        winConf      = WindowConfig size hints
        target       = RenderTarget (0,0) size factor 0.1 100 True
        res          = mempty
        rsettings     = RenderSettings RenderConfig
                        { _rcConfClearColor    = Color4 0.3 0.3 0.3 0
                        , _rcConfDebugNormals  = False
                        , _rcConfWireframe     = False
                        } target
    in do
        font  <- loadFont'
        
        let markup            = FontMarkup 1 1
            Right fontTexture = generateFontTexture font markup Monochrome fontchars fontAtlas
            helloTextE  = (textEntity3D fontTexture hellWorld 66) 
                            & textTransf.transPosition .~ V3 (-3) (7) (0) 
                            & textTransf.transScale    .~ (V3 1 1 1) / 10
            screenTextE = ReactingText $
                           (textEntity2D fontTexture "screen text (0_0)\nStumple" 77) 
                            & textTransf.transPosition .~ V3 (0) (100) (-0.5)
                            & textTransf.transScale    .~ V3 (1) (-1) (1)
                                                                  -- & entityScale .~ (V3 1 1 1) / 300
            floorE      = floorEntity & entityScale .~ 100 * V3 1 1 1
            scene'      = scene `addRenderable` floorE `addRenderable` helloTextE

        (_state', _sc, _gui) <- execApplication "MainWireless" conf
            $ basicWindowLoop winConf ((res, rsettings), scene', gui & sceneEntities .~ [SomeRenderable screenTextE]) loop

        return ()
        where
            loop _win inputState ((res, settings), scene, gui) = do
                let settingsNew      = settings `updateSettings` inputState
                    sceneNew         = scene `update3DScene` inputState `updateScene` inputState
                    guiNew           = gui `updateScene` inputState

                (resNew, _rlog) <- runRenderSystem
                                        [RenderUnit sceneNew, RenderUnit guiNew]
                                        settingsNew res

                return ((resNew, settingsNew), sceneNew, guiNew)
                --unless (isEmptyRenderLog l) $ mapM_ debugM $ rlog'log l
            loadFont' =
                let descr = FontDescriptor (21*64,21*64) (512,512)
                in loadFont fontPath descr

---------------------------------------------------------------------------------------------------
-- Input & Events


updateSettings :: RenderSettings -> InputState -> RenderSettings
updateSettings settings inputSt =
    settings & reRenderConfig.rcConfWireframe .~ inputSt `isPressed` Key'F1


update3DScene :: RenderScene -> InputState -> RenderScene
update3DScene scene inputSt =
    scene & sceneCamera      %~ cameraMovement inputSt
    where
        cameraMovement input cam =
            let moveV k v   = if input `isPressed` k then v else Linear.zero
                scalarD k v = if input `isPressed` k then v else 0
                speed       = 0.1 -- yes this is not frame independent
                movement    =  normalize $
                               moveV Key'W (V3 0 0 (-1))
                             + moveV Key'A (V3 (-1) 0 0)
                             + moveV Key'S (V3 0 0 1)
                             + moveV Key'D (V3 1 0 0)
                turn        =  scalarD Key'Q 1
                             + scalarD Key'E (-1)
                tilting     =  scalarD Key'R 1
                             + scalarD Key'F (-1)
                --fov         =  deg2rad $
                --               scalarD Key'X 1
                --             + scalarD Key'Z (-1)
                camHandle   = (cam^.cameraHandle)
                              `dolly` (speed * movement)
                              `pan`   turn
                              `tilt`  tilting
            in cam & cameraHandle .~ camHandle
                   -- & cameraFOV    +~ fov

updateScene :: RenderScene -> InputState -> RenderScene
updateScene scene inputSt =
    let boxUpdater = tryWithSomeRenderable (update inputSt :: Box -> Box)
        textUpdate = tryWithSomeRenderable (update inputSt :: ReactingText -> ReactingText)
    in scene & sceneEntities    %~ (map textUpdate) . (map boxUpdater) -- Not the efficent way :)

---------------------------------------------------------------------------------------------------
-- Entity Definitions

camPosition :: V3 Float
camPosition = V3 (0) 10 (10)
testScene :: RenderScene
testScene = fill $ emptyRenderScene (Camera3D (fpsCamera `dolly` camPosition `tilt` (-45)) (deg2rad 60.0))
    where
    fill scene =
        let box1     = Box $ boxEntity & entityScale .~ 1.5 & entityPosition .~ V3 (-3) 2 (0)
            box2     = Box $ boxEntity & entityScale .~ 1.5 & entityPosition .~ V3 3 2 (0)
        in scene 
            `addRenderable` box1 
            `addRenderable` box2

