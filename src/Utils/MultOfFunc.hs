{-# OPTIONS -fno-warn-name-shadowing #-}
{- |
Module      :  Main
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

Проверка корректности реализации функции умножения функции, которые задаются
в виде списка пар: значения аргумента, значение функции.

-}

module Main where

import Control.Applicative ( (<$>) )
import Control.Monad
import Data.Function
import Data.List
import System.Random
import Text.Printf

import Synth.Common ( (<*>), lerp )

main :: IO ()
main = do
  xs <- return [0..50]
  us <- return . sort =<< replicateM 50 (randomRIO (-10,60)) :: IO [Double]

  let xy = (\ x -> (x, exp(-x/40) * sin(x*pi/4)) ) <$> xs
      uv = (\ u -> (u, u/30) ) <$> us
      kl = (\ (u,v) -> (u, v * lerp xy u)) <$> uv

  mapM_ (\ ((x,y),(u,v), (k,l)) -> printf "%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\n" x y u v k l) $ zip3 xy uv kl

  putStrLn "# "

  mapM_ (\ ((x,y),(u,v)) -> printf "%.4f\t%.4f\t%.4f\t%.4f\n" x y u v) $ zip (xy <*> uv) (xy `mul` uv)


-- Альтернативная реализация умножения функции.
mul :: (Fractional a, Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]
mul [] _ = []
mul _ [] = []
mul [(_,y)] uv = ( \ (u,v) -> (u,y*v) ) <$> uv
mul xy [(_,v)] = ( \ (x,y) -> (x,v*y) ) <$> xy
mul xy uv = sortBy (compare `on` fst) $ (xy `mul'` uv) ++ (uv `mul'` xy)
      where
       mul' [] _ = []

       mul' ((x, y) : xy) [(u, v)]
         | u < x = (x, y*v) : ((\ (x, y) -> (x, y*v)) <$> xy)

       mul' xy@((x0,y0) : _) uv@((u0, v0) : (u1, v1) : _) 
         | x0 < u0 = (x0, y0 * v0) : mul' (tail xy) uv
         | u1 < x0 =  mul' xy (tail uv)
         | u0 <= x0 && x0 <= u1 = (x0, y0 * (v0 + (v1-v0)*k)) : mul' (tail xy)  uv
         where
           k = (x0-u0)/(u1-u0)
