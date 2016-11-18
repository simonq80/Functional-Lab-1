{-# LANGUAGE OverloadedStrings #-}
module Main where
import Svg
import Shapes
import Web.Scotty
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as B
import qualified Data.ByteString.Char8 as BS
import Prelude

main = scotty 3000 $ do
    get "/" $ do
        html $ "<form style=\"width:100%;\" method=\"post\" action=\"/svg\"><input name=\"desc\"><input type=\"submit\"></form><p>Examples:<br/>  [((Compose (Compose (Scale (Vector 4.1 4.1)) (Rotate 45)) (Translate (Vector 5 2))), Square, [(\"fillColor\", 4000), (\"borderWidth\", 0.1), (\"borderColor\", 128)])] <br/> [((Compose (Compose (Scale (Vector 4.1 4.1)) (Rotate 45)) (Translate (Vector 5 2))), Square, [(\"fillColor\", 4000), (\"borderWidth\", 0.1), (\"borderColor\", 128)]), ((Compose (Scale (Vector 3 3)) (Translate (Vector 3 3))), Circle, [(\"fillColor\", 25000)])]</p>"
    post "/svg" $ do
        str <- param "desc"
        html $ B.pack $ renderSvg $ toSvg $ stringToDrawing $ str


        
