{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Examples.CacheDemo where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as GraphicsVty

import qualified Brick.Types as BrickTypes
import qualified Brick.Main as BrickMain
import qualified Brick.Widgets.Center as BrickWidgetsCenter
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( vBox
  , padTopBottom
  , withDefAttr
  , cached
  , padBottom
  , str
  )
import Brick (on)
import Brick.Widgets.Center (hCenter)
import Brick.AttrMap (AttrName, attrMap)

data Name = ExpensiveWidget deriving (Ord, Show, Eq)

drawUi :: Int -> [Widget Name]
drawUi i = [ui]
  where
    ui = BrickWidgetsCenter.vCenter $
      vBox $ hCenter <$>
      [ str "This demo shows how cached widgets behave. The top widget below"
      , str "is cacheable, so once it's rendered, brick re-uses the rendering"
      , str "each time it is drawn. The bottom widget is not cacheable so it is"
      , str "a redraw of cached widgets; we can trigger that here with 'i'. Notice"
      , str "how state changes with '+' aren't reflected in the cached widget"
      , str "until the cache is invalidated with 'i'."
      , padTopBottom 1 $
        cached ExpensiveWidget $
        withDefAttr emphAttr $ str $ "This widget is cached (state = " <> show i <> ")"
      , padBottom (BrickTypes.Pad 1) $
        withDefAttr emphAttr $ str $ "This widget is not cached (state = " <> show i <> ")"
      , hCenter $ str "Press 'i' to invalidate the cache,"
      , str "'+' to change the state value, and"
      , str "'Esc' to quit."
      ]

emphAttr :: AttrName
emphAttr = "emphasis"

appEvent :: Int -> BrickEvent Name e -> BrickTypes.EventM Name (BrickTypes.Next Int)
appEvent i (VtyEvent (GraphicsVty.EvKey (GraphicsVty.KChar '+') [])) = BrickMain.continue $ i + 1
appEvent i (VtyEvent (GraphicsVty.EvKey (GraphicsVty.KChar 'i') [])) = BrickMain.invalidateCacheEntry ExpensiveWidget >> BrickMain.continue i
appEvent i (VtyEvent (GraphicsVty.EvKey GraphicsVty.KEsc [])) = BrickMain.halt i
appEvent i _ = BrickMain.continue i

app :: BrickMain.App Int e Name
app = BrickMain.App
  { BrickMain.appDraw = drawUi
  , BrickMain.appStartEvent = return
  , BrickMain.appHandleEvent = appEvent
  , BrickMain.appAttrMap = const $ attrMap GraphicsVty.defAttr [(emphAttr, GraphicsVty.white `on` GraphicsVty.blue)]
  , BrickMain.appChooseCursor = BrickMain.neverShowCursor
  }

main :: IO ()
main = void $ BrickMain.defaultMain app 0
