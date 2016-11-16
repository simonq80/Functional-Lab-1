{-# LANGUAGE OverloadedStrings #-}
module Main where
import Svg
import Shapes
import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $ do
    get "/" $ do
        html $ "<form method=\"post\" action=\"/svg\"><input name=\"desc\"><input type=\"submit\"></form>"
    post "/svg" $ do
        str <- body
        html $ showSvg $ toSvg $ stringToDrawing $ str


        
