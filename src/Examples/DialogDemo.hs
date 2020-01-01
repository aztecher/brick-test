{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Examples.DialogDemo where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as GraphicsVty

import qualified Brick.Main as BrickMain
import Brick.Types
  ( Widget
  , BrickEvent (..)
  )
import Brick.Widgets.Core
  ( padAll
  , str
  )
import qualified Brick.Widgets.Dialog as BrickWidgetsDialog
import qualified Brick.Widgets.Center as BrickWidgetsCenter
import qualified Brick.AttrMap as BrickAttrMap
import Brick.Util (on, bg)
import qualified Brick.Types as BrickTypes

data Choice = Red | Blue | Green deriving Show

drawUI :: BrickWidgetsDialog.Dialog Choice -> [Widget ()]
drawUI d = [ui]
  where
    ui = BrickWidgetsDialog.renderDialog d $ BrickWidgetsCenter.hCenter $ padAll 1 $ str "This is the dialog body."

appEvent :: BrickWidgetsDialog.Dialog Choice -> BrickEvent () e -> BrickTypes.EventM () (BrickTypes.Next (BrickWidgetsDialog.Dialog Choice))
appEvent d (VtyEvent ev) =
  case ev of
    GraphicsVty.EvKey GraphicsVty.KEsc [] -> BrickMain.halt d
    GraphicsVty.EvKey GraphicsVty.KEnter [] -> BrickMain.halt d
    _ -> BrickMain.continue =<< BrickWidgetsDialog.handleDialogEvent ev d
appEvent d _ = BrickMain.continue d

initialState :: BrickWidgetsDialog.Dialog Choice
initialState = BrickWidgetsDialog.dialog (Just "Title") (Just (0, choises)) 50
  where
    choises = [ ("Red", Red)
              , ("Blue", Blue)
              , ("Green", Green)
              ]

theMap :: BrickAttrMap.AttrMap
theMap = BrickAttrMap.attrMap GraphicsVty.defAttr
  [ (BrickWidgetsDialog.dialogAttr, GraphicsVty.white `on` GraphicsVty.blue)
  , (BrickWidgetsDialog.buttonAttr, GraphicsVty.black `on` GraphicsVty.white)
  , (BrickWidgetsDialog.buttonSelectedAttr, bg GraphicsVty.yellow)
  ]

theApp :: BrickMain.App (BrickWidgetsDialog.Dialog Choice) e ()
theApp =
  BrickMain.App { BrickMain.appDraw = drawUI
                , BrickMain.appChooseCursor = BrickMain.showFirstCursor
                , BrickMain.appHandleEvent = appEvent
                , BrickMain.appStartEvent = return
                , BrickMain.appAttrMap = const theMap
                }

main :: IO ()
main = do
  d <- BrickMain.defaultMain theApp initialState
  putStrLn $ "You chose: " <> show (BrickWidgetsDialog.dialogSelection d)


