{-# OPTIONS -Wall -Werror -fno-warn-unused-do-bind #-}

{- |
Module      :  Main
Description :  Программа для изменения (уменьшения) разрешения спектра 
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

Простая программа для уменьшения разрешения спектра. Разрешение спектра
уменьшается путем свертки с инструментальным контуром.  В качестве такого
контура используется функция Гаусса.  Дисперсия задается в виде
единственного аргумента при запуске программы.  Спектр считывается из stdin
и ожидается в виде строк, состоящих их двух чисел: длина волны в ангстремах
и поток.

-}

module Main where

import Control.Applicative
import System.Environment
import System.IO
import Text.Printf

main :: IO ()
main = do

 [s] <- getArgs

 xs <- return . ( (\[x,y] -> (x,y)) <$>) . ((read <$>) . (take 2 . words) <$>) . lines =<< hGetContents stdin

 mapM_ (\(x,y) -> printf "%8.5f %8.5f\n" x y ) (convolve (read s) xs)


convolve :: Double -> [(Double,Double)] -> [(Double,Double)]
convolve s xs = [ (x, quad [ (u, v * ker u x) | (u,v) <- uv ]) | (x,uv) <- pair n xs ]
 where
   ker u x = gauss s (u-x)

   n = 10 * ceiling s

gauss :: Double -> Double -> Double
gauss sigma x = 1 / (sigma * sqrt( 2*pi )) * exp( -0.5 * (x / sigma)**2 )

quad :: [ (Double,Double) ] -> Double
quad xy = 0.5 * (sum $ (\ ((x0,y0),(x1,y1)) -> (x1-x0)*(y1+y0) ) <$> zip xy (tail xy))

pair :: Int -> [ (Double,Double) ] -> [ (Double, [(Double,Double)]) ]
pair n xs =  drop n $ take (l-n) (pair' xs (hs++xs))
  where
   l  = length xs

   hs = replicate (n `div` 2) undefined

   pair' [] _            = []
   pair' ((x,_):[])  uvs = [(x, take n uvs)]
   pair' ((x,_):xys) uvs =  (x, take n uvs) : pair' xys (tail uvs)
