module Examples.HelloWorld where

import Brick

ui :: Widget ()
ui = str "Hello World"

main :: IO ()
main = simpleMain ui
