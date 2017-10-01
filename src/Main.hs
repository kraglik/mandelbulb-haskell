module Main (main) where

import Control.Parallel
import Control.Parallel.Strategies

cam = getCamera cameraPos cameraDir (0, 1, 0)
colors = render cam width height

cadd :: Color -> Color -> Color
cadd (a, b, c) (x, y, z) = (a + x, b + y, c + z)

sm = foldr1 cadd colors

pgm = make_pgm width height colors

lightDir = (0, -1, 0)

f1 = print sm
f2 = writeFile "fractal3.pgm" pgm

main = do
        f1
        f2

------------------------------------------------------------------------------------------------------

cameraPos = (-1, -0.3, 1)
cameraDir = normalize (1, 0, -1)
vX = (1, 0, 0)
vY = (0, 1, 0)

xMul = 1.0 :: Float--16/9 :: Float

width  = 2000
height = 2000

type Camera = (Point, Direction, Normal, Normal)

getCamera :: Point -> Direction -> Direction -> Camera
getCamera pos dir up = (pos, nDir, nvX, nvY)
    where nDir = normalize dir
          nvX  = normalize $ up `cross` dir
          nvY  = normalize $ nDir `cross` nvX


type Vector    = (Float, Float, Float)
type Point     = Vector
type Direction = Vector
type Ray       = (Point, Direction)
type Normal    = Direction
type Color     = Vector
type Distance  = Float

limit      = 128 :: Int
fPow       = 8  :: Float
shiftValue = 0.95 :: Float
epsilon    = 0.000001 :: Float
itLimit    = 400 :: Int

data Hit = Hit Point Direction Color Distance Int | None

fColor   = (1, 1, 0) :: Color

------------------------------------------------------------------------------------------------------

len :: Vector -> Float
len (x, y, z) = sqrt (x*x + y*y + z*z)

add :: Vector -> Vector -> Vector
add (x, y, z) (a, b, c) = (x + a, y + b, z + c)

sub :: Vector -> Vector -> Vector
sub (x, y, z) (a, b, c) = (x - a, y - b, z - c)

cross :: Vector -> Vector -> Vector
cross (x, y, z) (a, b, c) = (y * c - z * b, x * c - a * z, x * b - y * a)

dot :: Vector -> Vector -> Float
dot (x, y, z) (a, b, c) = x * a + y * b + z * c

sMul :: Vector -> Float -> Vector
sMul (x, y, z) m = (x * m, y * m, z * m)

sDiv :: Vector -> Float -> Vector
sDiv (x, y, z) d = (x / d, y / d, z / d)

normalize :: Vector -> Vector
normalize vec
    | l == 0 = (0, 0, 0)
    | otherwise = vec `sDiv` l
        where l = len vec

------------------------------------------------------------------------------------------------------

phi :: Vector -> Float
phi (x, y, z) = atan (y / x)

theta :: Vector -> Float
theta vec@(x, y, z) = acos (z / len vec)

powVec :: Vector -> Vector
powVec vec@(x, y, z) =
    let r  = len vec
        ph = phi vec
        th = theta vec
        nx = sin (fPow * th) * cos (fPow * ph)
        ny = sin (fPow * th) * sin (fPow * ph)
        nz = cos (fPow * th)
    in (nx, ny, nz) `sMul` (r**fPow)

fracNormal :: Point -> Direction -> Vector
fracNormal pos dir@(a, b, c) = (x, y, z)
        where
            x = distance (pos `add` (a, 0, 0)) - distance (pos `sub` (a, 0, 0))
            y = distance (pos `add` (0, b, 0)) - distance (pos `sub` (0, b, 0))
            z = distance (pos `add` (0, 0, c)) - distance (pos `sub` (0, 0, c))


iterateZ :: Float -> Vector -> Vector -> Int -> (Float, Float)
iterateZ dr z c level
    | level > limit = (r, dr)
    | r > 2     = (r, dr)
    | otherwise =  iterateZ drn zn c (level + 1)
        where
            r         = len z
            zn        = (powVec z) `add` c
            drn       = (r ** (fPow - 1)) * fPow * dr + 1.0


distance :: Point -> Float
distance pos =
               let (r, dr) = iterateZ 1.0 pos pos 0
               in 0.5 * log r * r / dr


------------------------------------------------------------------------------------------------------

shift :: Ray -> Float -> Ray
shift ray@(pos, dir) mul = (pos `add` (dir `sMul` (mul * shiftValue)), dir)

rayMarch :: Ray -> Float -> Int -> Hit
rayMarch ray@(pos, dir) pathLen level
    | level > itLimit = None
    | dist < epsilon  = Hit pos (fracNormal pos dir) fColor pathLen level
    | otherwise       = rayMarch nextRay (pathLen + len (npos `sub` pos))(level + 1)
        where
              dist    = distance pos
              nextRay@(npos, ndir) = shift ray dist

------------------------------------------------------------------------------------------------------

make_pgm :: Int -> Int -> [ Color ] -> String
make_pgm width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify(xs)
                  where stringify [] = ""
                        stringify ((r,g,b):xs) = show (round (r*255)) ++ " "
                                                 ++ show (round (g*255)) ++ " "
                                                 ++ show (round (b*255)) ++ " "
                                                 ++ stringify xs


colorize :: Hit -> Color
colorize None = (0.1, 0.1, 0.1)
colorize hit@(Hit pos dir col dist lvl) = col `sMul` m
        where
            m = 1.0 - ((fromIntegral lvl) / (fromIntegral itLimit)) :: Float


traceRay :: Camera -> Float -> Float -> Hit
traceRay (cPos, cDir, vX, vY) x y = rayMarch ray 0 0
    where ray = (cPos, normalize (cDir `add` (vX `sMul` (x * xMul) `add` (vY `sMul` y))))


render :: Camera -> Int -> Int -> [Color]
render cam w h = task `using` parList rdeepseq
    where mw = w `div` 2
          mh = h `div` 2
          convert val mid = fromIntegral (val - mid) / fromIntegral mid :: Float
          task = (parMap rwhnf) colorize [traceRay cam (convert a mw) (convert b mh) | a <- [1..w], b <- [1..h]]