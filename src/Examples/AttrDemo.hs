{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP#-}
module Examples.AttrDemo where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Graphics.Vty
  ( Attr, white, blue, cyan, green, red, yellow
  , black, withURL
  )
import Brick.Main
import Brick.Types (Widget)
import Brick.Widgets.Core 
  ( (<=>)
  , withAttr
  , vBox
  , str
  , hyperlink
  , modifyDefAttr
  )
import Brick.Util (on, fg)
import Brick.AttrMap (attrMap, AttrMap)

ui :: Widget n
ui = vBox
  [ str "This text uses the global default attribute."
  , withAttr "foundFull" $
    str "Specifying an attribute name means we look it up in the attribute tree"
  , (withAttr "foundFgOnly" $
    str ("When we find a value, we merge it with with its parent in the attribute")
    <=> str "name tree all the way to the root (the global default).")
  , withAttr "missing" $
    str "A missing attribute name just resume the search at its parent."
  , withAttr ("general" <> "specific") $
    str "In this way we build complete attribute values by using an inheritance scheme"
  , withAttr "foundFull" $
    str "You cacn override everything ..."
  , withAttr "foundFgOnly" $
    str " ... or only what you want to change and inherit the rest."
  , str "Attribute names are assembled with the Monoid append operation to indicate"
  , str "hierarchy levels, e.g. \"window\" <> \"title\"."
  , str " "
  , withAttr "linked" $
    str "This text is hyperlinked in terminals that support hyperlinking."
  , str " "
  , hyperlink "http://www.google.com/" $
    str "This text is also hyperlinked in terminals that support hyperlinking."
  , str " "
  , modifyDefAttr (`withURL` "http://www.google.com/") $
    str "This text is hyperlinked by modifying the default attribute."
  ]

globalDefault :: Attr
globalDefault = white `on` blue

theMap :: AttrMap
theMap = attrMap globalDefault
  [ ("foundFull",             white `on` green)
  , ("foundFgOnly",           fg red)
  , ("general",               yellow `on` black)
  , ("general" <> "specific", fg cyan)
  , ("linked",                fg yellow `withURL` "http://www.google.com/")
  ]

app :: App () e ()
app = App { appDraw = const [ui]
          , appHandleEvent = resizeOrQuit
          , appStartEvent = return
          , appAttrMap = const theMap
          , appChooseCursor = neverShowCursor
          }

main :: IO ()
main = defaultMain app ()
