module Main where
import Svg
import Shapes

main = do
    showSvg $ toSvg $ [(identity, circle, [("fillColor", 4000), ("borderColor", 0), ("borderWidth", 0.1)])]
