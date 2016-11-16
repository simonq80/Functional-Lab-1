module Shapes(
  Shape(..), Point, Vector, Transform, Stylesheet, Drawing,
  point, getX, getY, getTrans,
  empty, circle, square,
  identity, translate, rotate, scale, (<+>),
  )  where


-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

getFirst :: Matrix -> Vector
getFirst (Matrix a b) = a
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector


data Shape = Empty 
           | Circle 
           | Square
             deriving Show

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square

type Stylesheet = [(String, Double)]

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Double
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate angle 
t0 <+> t1 = Compose t0 t1

-- (tx, ty, sx, sy, r)
getTrans :: Transform -> (Double, Double, Double, Double, Double)
getTrans s = fixScale $ getTransformations s

fixScale :: (Double, Double, Double, Double, Double) -> (Double, Double, Double, Double, Double)
fixScale (a, b, 0, d, e) = fixScale (a, b, 1, d, e)
fixScale (a, b, c, 0, e) = fixScale (a, b, c, 1, e)
fixScale (a, b, c, d, e) = (a, b, c, d, e)

getTransformations :: Transform -> (Double, Double, Double, Double, Double)
getTransformations Identity = (0, 0, 0, 0, 0)
getTransformations (Translate (Vector tx ty)) = (tx, ty, 0, 0, 0)
getTransformations (Scale (Vector tx ty)) = (0, 0, tx, ty, 0)
getTransformations (Rotate m) = (0, 0, 0, 0, m)
getTransformations (Compose x y) = ((a1+a2), (b1+b2), (c1+c2), (d1+d2), (e1+e2))
    where (a1, b1, c1, d1, e1) = getTransformations x
          (a2, b2, c2, d2, e2) = getTransformations y 



-- Drawings

type Drawing = [(Transform,Shape,Stylesheet)]

