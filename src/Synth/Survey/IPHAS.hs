{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, TypeFamilies #-}

{- |
Module      :  Synth.Survey.IPHAS
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2012
License     :  BSD3

Maintainer  :  s.sichevskij@gmal.com
Stability   :  experimental
Portability :  portable

http://www.iphas.org/survey.shtml

-}

module Synth.Survey.IPHAS
 ( 
 ) where

import Control.Applicative  ( liftA2 )
import Control.Monad.Reader ( asks )

import Data.PhotoModel      ( PhotoModel( bands ), Band(..), RIHα(..) )
import Data.ZeroPoint       ( ZeroPoint( magFromFlux ) )
import Synth.Context        ( iphas ) 
import Synth.Common         ( quad, (<*>), hc ) 
import Synth.Photometry     ( ModelPhotometry(..) )
import Synth.Survey

instance ModelPhotometry (R IPHAS) where
  mag = do
     zp   <- asks $ zeroPoint . r'_band . bands . iphas
     r    <- asks $   pfilter . r'_band . bands . iphas

     let val f = R IPHAS $ Just ( magFromFlux zp n )
           where
             n = (quad $ r <*> f) / hc

     return val

instance ModelPhotometry (I IPHAS) where
  mag = do
     zp   <- asks $ zeroPoint . i'_band . bands . iphas
     i    <- asks $   pfilter . i'_band . bands . iphas

     let val f = I IPHAS $ Just ( magFromFlux zp n )
           where
             n = (quad $ i <*> f) / hc

     return val

instance ModelPhotometry (Hα IPHAS) where
  mag = do
     zp   <- asks $ zeroPoint . hα_band . bands . iphas
     hα   <- asks $   pfilter . hα_band . bands . iphas

     let val f = Hα IPHAS $ Just ( magFromFlux zp n )
           where
             n = (quad $ hα <*> f) / hc

     return val

instance ModelPhotometry (RHα IPHAS) where
  mag = do

     mr  <- mag
     mhα <- mag

     let c (R IPHAS r) (Hα IPHAS hα) = RHα IPHAS (r-hα)

     return $ liftA2 c mr mhα

instance ModelPhotometry (RI IPHAS) where
  mag = do

     mr <- mag
     mi <- mag

     let c (R IPHAS r) (I IPHAS i) = RI IPHAS (r-i)

     return $ liftA2 c mr mi
