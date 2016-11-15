module Main where
import Svg
import Shapes

main = do
    showSvg $ toSvg $ (identity, circle, (style 0 0 0)):[]
