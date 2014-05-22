{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, TypeFamilies #-}

{- |
Module      :  Synth.Survey.TWOMASS
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2012
License     :  BSD3

Maintainer  :  s.sichevskij@gmal.com
Stability   :  experimental
Portability :  portable

(2MASS Photometric System - Absolute Calibration)
http://www.ipac.caltech.edu/2mass/releases/allsky/doc/sec6_4a.html#rsr
http://www.ipac.caltech.edu/2mass/releases/sampler/ancillary/ptsource.format.html
http://www.ipac.caltech.edu/2mass/releases/sampler/explsup.html#contents (Photometric Calibration)
http://www.ipac.caltech.edu/2mass/releases/allsky/doc/sec4_8.html

-}

module Synth.Survey.TWOMASS
 ( 
 ) where

import Control.Applicative  ( liftA2 )
import Control.Monad.Reader ( asks )

import Data.PhotoModel      ( PhotoModel( bands ), Band(..), JHK(..) )
import Data.ZeroPoint       ( ZeroPoint( magFromFlux ) )
import Synth.Context        ( twomass ) 
import Synth.Common         ( quad, (<*>), hc ) 
import Synth.Photometry     ( ModelPhotometry(..) )
import Synth.Survey

instance ModelPhotometry (J TWOMASS) where
  mag = do
     zp   <- asks $ zeroPoint . j_band . bands . twomass
     j    <- asks $   pfilter . j_band . bands . twomass

     let val f = J TWOMASS $ Just ( magFromFlux zp n )
           where
             n = (quad $ j <*> f) / hc

     return val

instance ModelPhotometry (H TWOMASS) where
  mag = do
     zp   <- asks $ zeroPoint . h_band . bands . twomass
     h    <- asks $   pfilter . h_band . bands . twomass

     let val f = H TWOMASS $ Just ( magFromFlux zp n )
           where
             n = (quad $ h <*> f) / hc

     return val

instance ModelPhotometry (K TWOMASS) where
  mag = do
     zp   <- asks $ zeroPoint . k_band . bands . twomass
     k    <- asks $   pfilter . k_band . bands . twomass

     let val f = K TWOMASS $ Just ( magFromFlux zp n )
           where
             n = (quad $ k <*> f) / hc

     return val

instance ModelPhotometry (JH TWOMASS) where
  mag = do

     mj <- mag
     mh <- mag

     let c (J TWOMASS j) (H TWOMASS h) = JH TWOMASS (j-h)

     return $ liftA2 c mj mh

instance ModelPhotometry (JK TWOMASS) where
  mag = do

     mj <- mag
     mk <- mag

     let c (J TWOMASS j) (K TWOMASS k) = JK TWOMASS (j-k)

     return $ liftA2 c mj mk

instance ModelPhotometry (HK TWOMASS) where
  mag = do

     mh <- mag
     mk <- mag

     let c (H TWOMASS h) (K TWOMASS k) = HK TWOMASS (h-k)

     return $ liftA2 c mh mk
