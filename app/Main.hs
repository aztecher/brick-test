module Main where

import Brick
import qualified Examples.ReadmeDemo as ReadmeDemo
import qualified Examples.HelloWorld as HelloWorld
import qualified Examples.FillDemo as FillDemo
import qualified Examples.TextWrapDemo as TextWrapDemo
import qualified Examples.AttrDemo as AttrDemo
import qualified Examples.BorderDemo as BorderDemo
import qualified Examples.CacheDemo as CacheDemo
import qualified Examples.CustomEventDemo as CustomEventDemo
import qualified Examples.DialogDemo as DialogDemo
import qualified Examples.DynamicBorderDemo as DynamicBorderDemo
import qualified Examples.EditDemo as EditDemo
import qualified Examples.FileBrowserDemo as FileBrowserDemo
import qualified Examples.FormDemo as FormDemo
import qualified Examples.LayerDemo as LayerDemo
import qualified Examples.ListDemo as ListDemo
import qualified Examples.ListViDemo as ListViDemo
import qualified Examples.MarkupDemo as MarkupDemo
import qualified Examples.MouseDemo as MouseDemo
import qualified Examples.PaddingDemo as PaddingDemo
import qualified Examples.ProgressBarDemo as ProgressBarDemo
import qualified Examples.SuspendAndResumeDemo as SuspendAndResumeDemo
import qualified Examples.ViewportScrollDemo as ViewportScrollDemo
import qualified Examples.ThemeDemo as ThemeDemo
import qualified Examples.VisibilityDemo as VisibilityDemo

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
-- main = simpleMain ui
-- main = ReadmeDemo.main
-- main = FillDemo.main
-- main = HelloWorld.main
-- main = TextWrapDemo.main
-- main = AttrDemo.main
-- main = BorderDemo.main
-- main = CacheDemo.main
-- main = CustomEventDemo.main
-- main = DialogDemo.main
-- main = DynamicBorderDemo.main
-- main = EditDemo.main
-- main = FileBrowserDemo.main
-- main = FormDemo.main
-- main = LayerDemo.main
-- main = ListDemo.main
-- main = ListViDemo.main
-- main = MarkupDemo.main
-- main = MouseDemo.main
-- main = PaddingDemo.main
-- main = ProgressBarDemo.main
-- main = SuspendAndResumeDemo.main
-- main = ViewportScrollDemo.main
-- main = ThemeDemo.main
main = VisibilityDemo.main
