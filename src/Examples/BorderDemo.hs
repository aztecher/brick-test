{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Examples.BorderDemo where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative((<$>))
#endif

import Data.Monoid ((<>))
import qualified Data.Text as DataText
import qualified Graphics.Vty as GraphicsVty

import qualified Brick.Main as BrickMain
import Brick.Util (fg, on)
import qualified Brick.AttrMap as BrickAttrMap
import Brick.Types (Widget)

import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , withAttr
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , txt
  , str
  )
import qualified Brick.Widgets.Center as BrickWidgetCenter
import qualified Brick.Widgets.Border as BrickWidgetBorder
import qualified Brick.Widgets.Border.Style as BrickWidgetBorderStyle

styles :: [(DataText.Text, BrickWidgetBorderStyle.BorderStyle)]
styles = [ ("ascii", BrickWidgetBorderStyle.ascii)
         , ("unicode", BrickWidgetBorderStyle.unicode)
         , ("unicode bold", BrickWidgetBorderStyle.unicodeBold)
         , ("unicode rounded", BrickWidgetBorderStyle.unicodeRounded)
         , ("custom", custom)
         , ("from 'x'", BrickWidgetBorderStyle.borderStyleFromChar 'x')
         ]

custom :: BrickWidgetBorderStyle.BorderStyle
custom = BrickWidgetBorderStyle.BorderStyle {
    BrickWidgetBorderStyle.bsCornerTL = '/'
  , BrickWidgetBorderStyle.bsCornerTR = '\\'
  , BrickWidgetBorderStyle.bsCornerBR = '/'
  , BrickWidgetBorderStyle.bsCornerBL = '\\'
  , BrickWidgetBorderStyle.bsIntersectFull = '.'
  , BrickWidgetBorderStyle.bsIntersectL = '.'
  , BrickWidgetBorderStyle.bsIntersectR = '.'
  , BrickWidgetBorderStyle.bsIntersectT = '.'
  , BrickWidgetBorderStyle.bsIntersectB = '.'
  , BrickWidgetBorderStyle.bsHorizontal = '*'
  , BrickWidgetBorderStyle.bsVertical = '!'
}

borderDemos :: [Widget ()]
borderDemos = mkBorderDemo <$> styles

mkBorderDemo :: (DataText.Text, BrickWidgetBorderStyle.BorderStyle) -> Widget()
mkBorderDemo (styleName, sty) =
  withBorderStyle sty $
  BrickWidgetBorder.borderWithLabel (str "label") $
  vLimit 5 $
  BrickWidgetCenter.vCenter $
  txt $ "  " <> styleName <> " style "

titleAttr :: BrickAttrMap.AttrName
titleAttr = "title"

borderMappings :: [(BrickAttrMap.AttrName, GraphicsVty.Attr)]
borderMappings =
  [ ( BrickWidgetBorder.borderAttr, GraphicsVty.yellow `on` GraphicsVty.black)
  , (titleAttr,                     fg GraphicsVty.cyan)
  ]

colorDemo :: Widget ()
colorDemo = updateAttrMap (BrickAttrMap.applyAttrMappings borderMappings) $
            BrickWidgetBorder.borderWithLabel (withAttr titleAttr $ str "title") $
            hLimit 20 $
            vLimit 5 $
            BrickWidgetCenter.center $
            str "colors!"

ui :: Widget ()
ui = hBox borderDemos
  <=> BrickWidgetBorder.hBorder
  <=> colorDemo
  <=> BrickWidgetBorder.hBorderWithLabel (str "horizontal border label")
  <=> (BrickWidgetCenter.center (str "Left of vertical border")
    <+> BrickWidgetBorder.vBorder
    <+> BrickWidgetCenter.center (str "Right of vertical border"))

main :: IO ()
main = BrickMain.simpleMain ui
