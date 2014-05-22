{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{- |
Module      :  Synth.Common
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2012
License     :  BSD3

Maintainer  :  s.sichevskij@gmal.com
Stability   :  experimental
Portability :  portable

Определение различных типов и вспомогательных функций.

-}

module Synth.Common
 ( Rv(..), Av(..), Ebv(..), Teff(..), Logg(..), FeH(..), Vtur(..), Sp(..), Theta(..), Dist(..)
 , Spectrum
 , Extinction
 , quad
 , lerp
 , lerpL
 , (<*>)
 , hc
 ) where

import Control.Applicative ( (<$>) )
import Data.Function       ( on )
import Data.List           ( sortBy, sort )
import Data.NumInstances   ()
import Data.ZeroPoint      ( Flux )

newtype Rv    = Rv    Double deriving (Show, Read, Num, Eq, Ord, Enum, Fractional)
newtype Av    = Av    Double deriving (Show, Read, Num, Eq, Ord, Enum, Fractional)
newtype Ebv   = Ebv   Double deriving (Show, Read, Num, Eq, Ord, Enum, Fractional)
newtype Teff  = Teff  Double deriving (Show, Read, Num, Eq, Ord, Enum, Fractional) 
newtype Logg  = Logg  Double deriving (Show, Read, Num, Eq, Ord, Enum, Fractional)
newtype FeH   = FeH   Double deriving (Show, Read, Num, Eq, Ord, Enum, Fractional)
newtype Vtur  = Vtur  Double deriving (Show, Read, Num, Eq, Ord, Enum, Fractional)
newtype Sp    = Sp    Double deriving (Show, Read, Num, Eq, Ord, Enum, Fractional)
newtype Theta = Theta Double deriving (Show, Read, Num, Eq, Ord, Enum, Fractional)
newtype Dist  = Dist  Double deriving (Show, Read, Num, Eq, Ord, Enum, Fractional)

type Spectrum   = [ (Double, Double) ]
type Extinction = Double -> Double  

-- Произведение постоянной Планка на скорость света, эрг * А / с 
hc :: Double
hc = 6.62606957E-27 * 2.99792458E+18
 
-- Интегрирование спектра с целью вычисления потока. Интегрирование
-- выполняется численно методом трапеций.
quad :: Spectrum -> Flux
quad xy = 0.5 * sum $ (\ ((x0,y0),(x1,y1)) -> (x1-x0)*(y1+y0) ) <$> zip xy (tail xy)


-- Умножение двух функций заданных виде списка пар: значение аргумента,
-- значение функции.  В итоге получается аналогичный список, содержащий
-- значения аргументов как первой, так и второй функции, и соответствующее
-- им произведения значений функции.  Промежуточные значения функций
-- вычисляются линейной интерполяцией.
(<*>) :: (Ord a, Fractional a) => [(a, a)] -> [(a,a)] -> [(a,a)]
{--
(<*>) xy uv = (\ x -> (x, (fy * fv) x)) <$> ( sort $ fst <$> (xy ++ uv) )
  where
    -- Из списка пар значений аргумента и функции путем линейной
    -- интерполяции определяем функции.
    fy = lerp xy
    fv = lerp uv
--}
(<*>) xy uv = zip xu $ zipWith (*) (fy xu) (fv xu)
  where
    -- Из списка пар значений аргумента и функции путем линейной
    -- интерполяции определяем функции.  Для повышения надежности и
    -- избавления от потенциальной ошибки выполняем сортировку по значению
    -- аргумента функции.  Хотя это очевидным образом уменьшает
    -- производительность.
    fy = lerpL $ sortBy (compare `on` fst) xy
    fv = lerpL $ sortBy (compare `on` fst) uv

    xu = sort $ fst <$> (xy ++ uv)

-- Вычисление путем линейной интерполяции значения функции, заданной виде
-- списка пар: значение аргумента, значение функции.
--
-- ВАЖНО!
-- Список должен быть отсортирован по возрастанию значения аргумента
-- функции.  Вне диапазона значений аргумента, определенного входным
-- списком, значения функции приравниваются соответствующим граничным
-- значениям.
lerp :: (Ord a, Fractional a) => [(a, a)] -> a -> a
lerp [(_,y0)] _ = y0
lerp ((x0,y0):(x1,y1):xy) x
 | x0 < x && x < x1 = y0 + (y1-y0)*(x-x0)/(x1-x0)
 | x <= x0 = y0 
 | otherwise = lerp ((x1,y1):xy) x

-- Списковый вариант функции lerp.
--
-- ВАЖНО!
-- Список аргументов должен быть отсортирован по возрастанию.
lerpL :: (Ord a, Fractional a) => [(a, a)] -> [a] -> [a]
lerpL [(_,y0)] xs = replicate (length xs) y0
lerpL xy@((x0,y0):(x1,y1):_) xs@(x:_) 
 | x0 < x && x < x1 = (y0 + (y1-y0)*(x-x0)/(x1-x0)) : ys
 | x <= x0 = y0 : ys
 | otherwise = lerpL (tail xy) xs
 where 
   ys = lerpL xy (tail xs)
