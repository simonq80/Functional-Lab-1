{-# LANGUAGE OverloadedStrings #-}
module Svg where
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Shapes
import Numeric (showHex)

showSvg :: S.Svg -> IO()
showSvg s = do
        let a = renderSvg s
        putStrLn a
        writeFile "/users/ugrad/siquigle/Functional/Lab1/out.svg" a 


stringToDrawing :: String -> Drawing
stringToDrawing s = read s

toSvg :: Drawing -> S.Svg
toSvg d = S.docTypeSvg ! A.version "1.1" ! A.width "1000" ! A.height "1000" ! A.viewbox "0 0 10 10" $ drawingToSvg d



drawingToSvg :: Drawing -> S.Svg
drawingToSvg d = case d of
    x:[] -> (shapeToSvg $ head d)  
    x:xs -> (shapeToSvg $ head d) >> (drawingToSvg $ tail d)
    



shapeToSvg :: (Transform, Shape, Stylesheet)-> S.Svg
shapeToSvg (trans, Circle, style) = foldl (!) S.circle ([(A.cx "0"), (A.cy "0") , (A.r "1")]++(transToSvg trans)++(stylesToSvg style))
shapeToSvg (trans, Square, style) = foldl (!) S.rect ([(A.width "1"), (A.height "1"), (A.x "0"), (A.y "0")]++(transToSvg trans)++(stylesToSvg style))
shapeToSvg (_, _, _) = S.rect 

transToSvg :: Transform -> [S.Attribute]
transToSvg t = [A.transform (S.stringValue $ ("translate("++(show a) ++ " " ++ (show b) ++ ") scale(" ++ (show c) ++ " " ++ (show d) ++ ") rotate(" ++ (show e) ++ " 0 0)"))]
    where (a, b, c, d, e) = getTrans t

stylesToSvg :: Stylesheet -> [S.Attribute]
stylesToSvg s = map styleToSvg s

styleToSvg :: (String,Double) -> S.Attribute
styleToSvg ("fillColor", x) = A.fill (mkColor x)
styleToSvg ("borderColor", x) = A.stroke (mkColor x)
styleToSvg ("borderWidth", x) = A.strokeWidth (S.stringValue $ show x)
styleToSvg (_, _) = A.fontSize "1" 

mkColor :: Double -> S.AttributeValue
mkColor c = S.stringValue $ "#" ++  (setLength (showHex (floor c) ""))

setLength :: String -> String
setLength s = if (length s < 6)
    then setLength $ "0" ++ s
    else take 6 s
