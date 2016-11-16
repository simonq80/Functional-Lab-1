module Main where
import Svg
import Shapes

main = do
    st <- readFile "/users/ugrad/siquigle/Functional/Lab1/in.txt"
    putStrLn st
    showSvg $ toSvg $ stringToDrawing $ st
