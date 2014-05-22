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

-}

module Synth.Survey.LIRA
 ( 
 ) where

import Control.Monad.Reader ( asks )

import Data.PhotoModel      ( PhotoModel( bands ), Band(..), Lira(..) )
import Data.ZeroPoint       ( ZeroPoint( magFromFlux ) )
import Synth.Context        ( lira ) 
import Synth.Common         ( quad, (<*>), hc ) 
import Synth.Photometry     ( ModelPhotometry(..) )
import Synth.Survey

instance ModelPhotometry (Q195 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q195_band . bands . lira
     t    <- asks $   pfilter . q195_band . bands . lira

     let val f = Q195 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q218 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q218_band . bands . lira
     t    <- asks $   pfilter . q218_band . bands . lira

     let val f = Q218 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q270 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q270_band . bands . lira
     t    <- asks $   pfilter . q270_band . bands . lira

     let val f = Q270 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q350 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q350_band . bands . lira
     t    <- asks $   pfilter . q350_band . bands . lira

     let val f = Q350 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q374C LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q374c_band . bands . lira
     t    <- asks $   pfilter . q374c_band . bands . lira

     let val f = Q374C LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q440 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q440_band . bands . lira
     t    <- asks $   pfilter . q440_band . bands . lira

     let val f = Q440 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q550 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q550_band . bands . lira
     t    <- asks $   pfilter . q550_band . bands . lira

     let val f = Q550 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q700 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q700_band . bands . lira
     t    <- asks $   pfilter . q700_band . bands . lira

     let val f = Q700 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q785C LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q785c_band . bands . lira
     t    <- asks $   pfilter . q785c_band . bands . lira

     let val f = Q785C LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q825 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q825_band . bands . lira
     t    <- asks $   pfilter . q825_band . bands . lira

     let val f = Q825 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q930 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q930_band . bands . lira
     t    <- asks $   pfilter . q930_band . bands . lira

     let val f = Q930 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (Q1000 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q1000_band . bands . lira
     t    <- asks $   pfilter . q1000_band . bands . lira

     let val f = Q1000 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (QPN14 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q_pn14_band . bands . lira
     t    <- asks $   pfilter . q_pn14_band . bands . lira

     let val f = QPN14 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

instance ModelPhotometry (QPN30 LIRA) where
  mag = do
     zp   <- asks $ zeroPoint . q_pn30_band . bands . lira
     t    <- asks $   pfilter . q_pn30_band . bands . lira

     let val f = QPN30 LIRA $ Just ( magFromFlux zp n )
           where
             n = (quad $ t <*> f) / hc

     return val

