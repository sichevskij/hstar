{-# OPTIONS  -fno-warn-name-shadowing -fno-warn-incomplete-patterns #-}
{-# LANGUAGE TypeOperators #-}
{- |
Module      :  Synth.Extinction.CCM
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2012
License     :  BSD3

Maintainer  :  s.sichevskij@gmal.com
Stability   :  experimental
Portability :  portable

Модель закона межзвездного поглощения

Cardelli, Clayton, & Mathis (1989) derived a mean extinction law 
(for 0.12 μm < λ < 3.5 μm) that depends on only one parameter RV = AV /E(B − V).

-}

module Synth.Extinction.CCM
 ( model
 ) where

import Data.HList           ( (:::)(..), Null(..) )
import Synth.Common         ( Av(..), Rv(..), Extinction )

-- | Cardelli, Clayton, & Mathis (1989, hereafter CCM) derived a mean
-- extinction law (for 0.12 μm < λ < 3.5 μm) that depends on only one
-- parameter RV = AV /E(B − V).
ielaw :: Double -> Double -> Extinction
ielaw av rv = law where

 lb = 1250  -- правая граница закона ≡ 1/0.8*1E4
 rb = 33333 -- левая граница закона  ≡ 1/0.3*1E4

 law wl
  | wl < lb = law lb
  | wl > rb = law rb
  | otherwise = av * ( a(x) + b(x) /rv )
   where
     a x  | 0.3 <= x && x <= 1.1 = 0.574 * x**1.61
          | 1.1 <= x && x <= 3.3 = 1 + y * (0.17699 + y * (-0.50447  + y * (-0.02427 + y * (0.72085 + y * (0.01979 + y * (-0.77530 + y * 0.32999))))))
          | 3.3 <= x && x <= 8.0 = 1.752 - 0.316*x - 0.104/((x - 4.67)**2 + 0.341) + fa(x)

     b x  | 0.3 <= x && x <= 1.1 = -0.527 * x**1.61
          | 1.1 <= x && x <= 3.3 = y * (1.41338 + y * (2.28305 + y * (1.07233 + y * (-5.38434 + y * (-0.62251 + y * (5.30260 - y * 2.09002))))))
          | 3.3 <= x && x <= 8.0 = -3.090 + 1.825*x + 1.206/((x - 4.62)**2 + 0.263) + fb(x)

     fa x | 8.0 >= x && x >= 5.9 = (x - 5.9)**2 * (-0.04473 - 0.009779*(x - 5.9)) -- -0.04473*(x - 5.9)**2 - 0.009779*(x - 5.9)**3
          | otherwise = 0

     fb x | 8.0 >= x && x >= 5.9 = (x - 5.9)**2 * (0.21300  + 0.120700*(x - 5.9))   --0.21300*(x - 5.9)**2 + 0.120700*(x - 5.9)**3
          | otherwise = 0

     x  = 1 / um
     y  = x - 1.82

     -- ангстремы в микроны
     um = wl * 1E-4

model :: Monad m => m ((Av ::: Rv ::: Null) ->  Extinction)
model = do

  let ext (Av av ::: Rv rv ::: Null) = 10**(-0.4 * aie)
       where
         aie = ielaw av rv

  return ext
