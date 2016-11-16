module Main where
import Svg
import Shapes

main = do
    showSvg $ toSvg $ [(((scale (point 4.1 4.1)) <+> (rotate 45)<+> (translate (point 5 5))), circle, [("fillColor", 4000), ("borderColor", 0), ("borderWidth", 0.1)]), (((scale (point 4.1 4.1)) <+> (rotate 45)<+> (translate (point 5 5))), square, [("fillColor", 4000), ("borderColor", 0), ("borderWidth", 0.1)])]
