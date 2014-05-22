{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}
{- |
Module      :  Data.ZeroPoint
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

This class is used to characterize a zero point flux obtained during the
calibration of a certain photometry filter on a certain photometric system
configuration.  In the ZeroPoint class we define two conversion functions:
magFromFlux and fluxFromMag.

-}

module Data.ZeroPoint
 ( ZeroPoint(..)
 , AsinhZeroPoint(..)
 , PogsonZeroPoint(..)
 , LinearFluxZeroPoint(..)
 , Flux
 , Mag 
 ) where


type Flux = Double
type Mag  = Double
type SofteningParameter = Double

data AsinhZeroPoint      = AsinhZeroPoint  Flux Mag SofteningParameter
data PogsonZeroPoint     = PogsonZeroPoint Flux Mag
data LinearFluxZeroPoint = LinearFluxZeroPoint Flux Mag

-- This class is used to characterize a zero point flux obtained during the
-- calibration of a certain photometry filter on a certain photometric system
-- configuration
class ZeroPoint a where
  magFromFlux :: a -> Flux -> Mag
  fluxFromMag :: a -> Mag -> Flux

--
instance ZeroPoint AsinhZeroPoint where
  magFromFlux zp f =  m0 - 2.5/log 10 * ( asinh( f/f0/2/b ) + log b )
    where
      AsinhZeroPoint f0 m0 b = zp

  fluxFromMag zp m = f0 * 10**( 0.4*(m0-m) ) * ( 1 - b*b * 10**( 2/2.5 * (m-m0) ) )
    where
      AsinhZeroPoint f0 m0 b = zp

instance ZeroPoint PogsonZeroPoint where
  magFromFlux zp f = m0 - 2.5 * log (f/f0) / log 10
    where
      PogsonZeroPoint f0 m0 = zp

  fluxFromMag zp m = f0 * 10**( 0.4*(m0-m) )
    where
      PogsonZeroPoint f0 m0 = zp

{--
instance ZeroPoint LinearFluxZeroPoint where
  magFromFlux zp f = undefined
  fluxFromMag zp m = undefined
--}
