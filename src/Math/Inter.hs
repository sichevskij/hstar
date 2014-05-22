{-# LANGUAGE TypeFamilies #-}
{- |
Module      :  Math.Inter
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2012
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}


module Math.Inter
 ( lerp
 , biLerp
 , triLerp
 , quaLerp
 , quiLerp
 , lerpR
 )
   where


import Data.AdditiveGroup ( (^+^), (^-^) )
import Data.VectorSpace   ( VectorSpace, Scalar, (*^), )
import GHC.Float          ( int2Double )

foreign import ccall unsafe "math.h floor"
    c_floor :: Double -> Double

floor' :: Double -> Int
{-# INLINE floor' #-}
floor' x = (truncate :: Double -> Int) (c_floor x)

class Interpolation a where
  type R a :: *
  lerpR :: a -> R a

instance ( Int ~ a
         , Interpolation b , VectorSpace b
         , Double ~ (Scalar b)
         ) 
      =>  Interpolation ( a -> b ) where
   type R (a -> b) = Double -> R b
   lerpR g = lerpR.lerp g


instance Interpolation Double where
   type R Double = Double
   lerpR a = a

-- |Linear interpolation
lerp :: (VectorSpace v, Double ~ Scalar v) => (Int -> v) -> (Double -> v)
lerp g x | abs k <= 1e-10 = f
         | otherwise      = f ^+^ k *^ (f' ^-^ f)
  where
    f  = g $ x'
    f' = g $ x' + 1
    x' = floor' x
    k  = x - int2Double x'

-- |Bilinear interpolation
biLerp :: (VectorSpace v, Scalar v ~ Double) => (Int -> Int -> v) -> Double -> Double -> v
biLerp f  = lerp . lerp f

-- |Trilinear interpolation
triLerp :: (VectorSpace v, Scalar v ~ Double) => (Int -> Int -> Int -> v) -> Double -> Double -> Double -> v
triLerp f  = biLerp . lerp f

-- |
quaLerp :: (VectorSpace v, Scalar v ~ Double) => (Int -> Int -> Int -> Int -> v) -> Double -> Double -> Double -> Double -> v
quaLerp f  = triLerp . lerp f

{--
quiLerp :: (VectorSpace v, Scalar v ~ Double) => (Int -> Int -> Int -> Int -> Int -> v) -> Double -> Double -> Double -> Double -> Double -> v
quiLerp f  = quaLerp . lerp f
--}
--{--
quiLerp :: (Show v, Num v, VectorSpace v, Scalar v ~ Double) => (Int -> Int -> Int -> Int -> Int -> v) -> Double -> Double -> Double -> Double -> Double -> v
quiLerp f x y z u v = fxyzuv
 where
    fxyzuv = fxyzu0 + (v-v0)*^(fxyzu1 - fxyzu0)
    fxyzu0 = fxyz00 + (u-u0)*^(fxyz10 - fxyz00)
    fxyzu1 = fxyz01 + (u-u0)*^(fxyz11 - fxyz01)

    fxyz00 = fxy000 + (z-z0)*^(fxy100 - fxy000)
    fxyz10 = fxy010 + (z-z0)*^(fxy110 - fxy010)
    fxyz01 = fxy001 + (z-z0)*^(fxy101 - fxy001)
    fxyz11 = fxy011 + (z-z0)*^(fxy111 - fxy011)
  
    fxy000 = fx0000 + (y-y0)*^(fx1000 - fx0000)
    fxy001 = fx0001 + (y-y0)*^(fx1001 - fx0001)
    fxy010 = fx0010 + (y-y0)*^(fx1010 - fx0010)
    fxy011 = fx0011 + (y-y0)*^(fx1011 - fx0011)
    fxy100 = fx0100 + (y-y0)*^(fx1100 - fx0100)
    fxy101 = fx0101 + (y-y0)*^(fx1101 - fx0101)
    fxy110 = fx0110 + (y-y0)*^(fx1110 - fx0110)
    fxy111 = fx0111 + (y-y0)*^(fx1111 - fx0111)

    fx0000 = f00000 + (x-x0)*^(f10000 - f00000)
    fx0001 = f00001 + (x-x0)*^(f10001 - f00001)
    fx0010 = f00010 + (x-x0)*^(f10010 - f00010)
    fx0011 = f00011 + (x-x0)*^(f10011 - f00011)
    fx0100 = f00100 + (x-x0)*^(f10100 - f00100)
    fx0101 = f00101 + (x-x0)*^(f10101 - f00101)
    fx0110 = f00110 + (x-x0)*^(f10110 - f00110)
    fx0111 = f00111 + (x-x0)*^(f10111 - f00111)
    fx1000 = f01000 + (x-x0)*^(f11000 - f01000)
    fx1001 = f01001 + (x-x0)*^(f11001 - f01001)
    fx1010 = f01010 + (x-x0)*^(f11010 - f01010)
    fx1011 = f01011 + (x-x0)*^(f11011 - f01011)
    fx1100 = f01100 + (x-x0)*^(f11100 - f01100)
    fx1101 = f01101 + (x-x0)*^(f11101 - f01101)
    fx1110 = f01110 + (x-x0)*^(f11110 - f01110)
    fx1111 = f01111 + (x-x0)*^(f11111 - f01111)

    f00000 = f x0i y0i z0i u0i v0i
    f00001 = f x0i y0i z0i u0i v1i
    f00010 = f x0i y0i z0i u1i v0i
    f00011 = f x0i y0i z0i u1i v1i
    f00100 = f x0i y0i z1i u0i v0i
    f00101 = f x0i y0i z1i u0i v1i
    f00110 = f x0i y0i z1i u1i v0i
    f00111 = f x0i y0i z1i u1i v1i
    f01000 = f x0i y1i z0i u0i v0i
    f01001 = f x0i y1i z0i u0i v1i
    f01010 = f x0i y1i z0i u1i v0i
    f01011 = f x0i y1i z0i u1i v1i
    f01100 = f x0i y1i z1i u0i v0i
    f01101 = f x0i y1i z1i u0i v1i
    f01110 = f x0i y1i z1i u1i v0i
    f01111 = f x0i y1i z1i u1i v1i
    f10000 = f x1i y0i z0i u0i v0i
    f10001 = f x1i y0i z0i u0i v1i
    f10010 = f x1i y0i z0i u1i v0i
    f10011 = f x1i y0i z0i u1i v1i
    f10100 = f x1i y0i z1i u0i v0i
    f10101 = f x1i y0i z1i u0i v1i
    f10110 = f x1i y0i z1i u1i v0i
    f10111 = f x1i y0i z1i u1i v1i
    f11000 = f x1i y1i z0i u0i v0i
    f11001 = f x1i y1i z0i u0i v1i
    f11010 = f x1i y1i z0i u1i v0i
    f11011 = f x1i y1i z0i u1i v1i
    f11100 = f x1i y1i z1i u0i v0i
    f11101 = f x1i y1i z1i u0i v1i
    f11110 = f x1i y1i z1i u1i v0i
    f11111 = f x1i y1i z1i u1i v1i

    x0i = floor' x
    y0i = floor' y
    z0i = floor' z
    u0i = floor' u
    v0i = floor' v

    x1i = x0i + 1
    y1i = y0i + 1
    z1i = z0i + 1
    u1i = u0i + 1
    v1i = v0i + 1

    x0 = int2Double x0i
    y0 = int2Double y0i
    z0 = int2Double z0i
    u0 = int2Double u0i
    v0 = int2Double v0i

--    x1 = int2Double x1i
--    y1 = int2Double y1i
--    z1 = int2Double z1i
--    u1 = int2Double u1i
--    v1 = int2Double v1i
--}
