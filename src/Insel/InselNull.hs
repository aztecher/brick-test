{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Insel.InselNull where

import Lens.Micro ((^.))
import Lens.Micro.TH

import qualified Graphics.Vty as GraphicsVty
import Brick
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , handleFormEvent
  , focusedFormInputAttr
  , (@@=)
  , radioField
  , renderForm
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )

import qualified Brick.Widgets.Edit as BEdit
import qualified Brick.Widgets.Border as BBorder
import qualified Brick.Widgets.Core as BCore
import qualified Brick.Widgets.Center as BCenter

-- border
import qualified Brick.Widgets.Border as BBorder

data Field = JopsdbField
           | AllField
           | LinksField
           deriving (Eq, Ord, Show)

data Place = Jopsdb
           | All
           | Links
           deriving (Eq, Show)

data PlaceInfo = PlaceInfo { _place :: Place } deriving (Show)
makeLenses ''PlaceInfo

mkForm :: PlaceInfo -> Form PlaceInfo e Field
mkForm = newForm ffc
  where
    ffc = [ radioField place [ (Jopsdb, JopsdbField, "Jopsdb")
                           , (All, AllField, "All")
                           , (Links, LinksField, "Links")
                           ]
          ]

ui :: Form PlaceInfo e Field -> [Widget Field]
ui f = [ui]
  where
    ui = BCore.freezeBorders $ BCore.vBox
      [ BBorder.borderWithLabel (BBorder.vBorder <+> str " Label " <+> BBorder.vBorder) $ BCore.hBox
        [ str "Wellcome Deep Place"
        , BCenter.vCenter $ BCenter.hCenter form
        ]
      ]
    form = renderForm f

theMap :: AttrMap
theMap = attrMap GraphicsVty.defAttr
  [ (focusedFormInputAttr, GraphicsVty.black `on` GraphicsVty.yellow)
  ]

draw :: Form PlaceInfo e Field -> [Widget Field]
draw f = [BCenter.vCenter $ BCenter.hCenter form <=> BCenter.hCenter help]
  where
    form = BBorder.border $ padTop (Pad 1) $ renderForm f
    help = padTop (Pad 1) $ BBorder.borderWithLabel (str "Help") (str "This is help message")

    -- handleEvent :: Form PlaceInfo e Field -> BrickEvent n e -> EventM n (Next (Form PlaceInfo e Field))
handleEvent state (VtyEvent (GraphicsVty.EvResize {})) = continue state
handleEvent state (VtyEvent (GraphicsVty.EvKey GraphicsVty.KEsc [])) = halt state
handleEvent state (VtyEvent (GraphicsVty.EvKey GraphicsVty.KEnter [])) = halt state
handleEvent state ev = do
  s' <- handleFormEvent ev state
  continue s'

app :: App (Form PlaceInfo e Field) e Field
app = App
  { appDraw = ui
  , appHandleEvent = handleEvent
  , appChooseCursor = focusRingCursor formFocus
  , appStartEvent = return
  , appAttrMap = const $ theMap
  }

main :: IO ()
main = do
  let buildVty = do
        v <- GraphicsVty.mkVty =<< GraphicsVty.standardIOConfig
        GraphicsVty.setMode (GraphicsVty.outputIface v) GraphicsVty.Mouse True
        return v

      initialPlaceInfo = PlaceInfo { _place = Jopsdb }
      form = mkForm initialPlaceInfo

  initialVty <- buildVty
  form' <- customMain initialVty buildVty Nothing app form
  putStrLn "the starting form state was: "
  print initialPlaceInfo

  putStrLn "the final form state was: "
  print $ formState form'


