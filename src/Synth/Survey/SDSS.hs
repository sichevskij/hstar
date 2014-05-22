{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators
           , TypeFamilies
           , FlexibleInstances
 #-}

{- |
Module      :  Synth.Survey.SDSS
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2012
License     :  BSD3

Maintainer  :  s.sichevskij@gmal.com
Stability   :  experimental
Portability :  portable

The SDSS has designed, defined, and calibrated its own photometric system
(Fukugita et al.  1996; Gunn et al.  1998; Smith et al.  2002), which is
characterized by the following particularities: (1) the use of a modified
Thuan-Gunn broadband filter system called ugriz; (2) a zero-point definition
in the AB magnitude system; (3) the use of a modified, nonlogarithmic,
definition of the magnitude scale (Lupton et al.  1999).

Magnitudes within the SDSS are expressed as inverse hyperbolic sine (or
"asinh") magnitudes, described in detail by Lupton, Gunn & Szalay (1999). 
They are sometimes referred to informally as luptitudes.

All calibrated magnitudes in the SDSS photometric catalogs are given not as
conventional Pogson astronomical magnitudes, but as asinh magnitudes
(http://www.sdss.org/dr7/algorithms/photometry.html)

-}

module Synth.Survey.SDSS () where


import Control.Applicative  ( liftA2 )
import Control.Monad.Reader ( asks )
import Data.PhotoModel      ( PhotoModel( bands ), Band(..), UGRIZ(..) )
import Data.ZeroPoint       ( ZeroPoint( magFromFlux ) )
import Synth.Context        ( sdss )
import Synth.Common         ( quad, (<*>), hc )
import Synth.Photometry     ( ModelPhotometry(..) )
import Synth.Survey

instance ModelPhotometry (U SDSS) where
  mag = do

     zp   <- asks $ zeroPoint . u_band . bands . sdss
     u    <- asks $   pfilter . u_band . bands . sdss
   
     let m f =  U SDSS $ Just (magFromFlux zp n)
          where
            n = (quad $ u <*> f) / hc

     return m

instance ModelPhotometry (G SDSS) where
  mag = do

     zp   <- asks $ zeroPoint . g_band . bands . sdss
     g    <- asks $   pfilter . g_band . bands . sdss
   
     let m f =  G SDSS $ Just (magFromFlux zp n)
          where
            n = (quad $ g <*> f) / hc

     return m

instance ModelPhotometry (R SDSS) where
  mag = do

     zp   <- asks $ zeroPoint . r_band . bands . sdss
     r    <- asks $   pfilter . r_band . bands . sdss
   
     let m f =  R SDSS $ Just (magFromFlux zp n)
          where
            n = (quad $ r <*> f) / hc

     return m

instance ModelPhotometry (I SDSS) where
  mag = do

     zp   <- asks $ zeroPoint . i_band . bands . sdss
     i    <- asks $   pfilter . i_band . bands . sdss
   
     let m f =  I SDSS $ Just (magFromFlux zp n)
          where
            n = (quad $ i <*> f) / hc

     return m

instance ModelPhotometry (Z SDSS) where
  mag = do

     zp   <- asks $ zeroPoint . z_band . bands . sdss
     z    <- asks $   pfilter . z_band . bands . sdss
   
     let m f =  Z SDSS $ Just (magFromFlux zp n)
          where
            n = (quad $ z <*> f) / hc

     return m

-- GU RU IU ZU

instance ModelPhotometry (GU SDSS) where
  mag = do

     mu <- mag
     mg <- mag

     let c (G SDSS g) (U SDSS u) = GU SDSS (g-u)

     return $ liftA2 c mg mu

instance ModelPhotometry (RU SDSS) where
  mag = do

     mu <- mag
     mr <- mag

     let c (R SDSS r) (U SDSS u) = RU SDSS (r-u)

     return $ liftA2 c mr mu

instance ModelPhotometry (IU SDSS) where
  mag = do

     mu <- mag
     mi <- mag

     let c (I SDSS i) (U SDSS u) = IU SDSS (i-u)

     return $ liftA2 c mi mu

instance ModelPhotometry (ZU SDSS) where
  mag = do

     mu <- mag
     mz <- mag

     let c (Z SDSS z) (U SDSS u) = ZU SDSS (z-u)

     return $ liftA2 c mz mu

-- RG IG ZG

instance ModelPhotometry (RG SDSS) where
  mag = do

     mg <- mag
     mr <- mag

     let c (R SDSS r) (G SDSS g) = RG SDSS (r-g)

     return $ liftA2 c mr mg

instance ModelPhotometry (IG SDSS) where
  mag = do

     mg <- mag
     mi <- mag

     let c (I SDSS i) (G SDSS g) = IG SDSS (i-g)

     return $ liftA2 c mi mg

instance ModelPhotometry (ZG SDSS) where
  mag = do

     mg <- mag
     mz <- mag

     let c (Z SDSS z) (G SDSS g) = ZG SDSS (z-g)

     return $ liftA2 c mz mg

-- IR ZR

instance ModelPhotometry (IR SDSS) where
  mag = do

     mr <- mag
     mi <- mag

     let c (I SDSS i) (R SDSS r) = IR SDSS (i-r)

     return $ liftA2 c mi mr

instance ModelPhotometry (ZR SDSS) where
  mag = do

     mr <- mag
     mz <- mag

     let c (Z SDSS z) (R SDSS r) = ZR SDSS (z-r)

     return $ liftA2 c mz mr

-- ZI

instance ModelPhotometry (ZI SDSS) where
  mag = do

     mi <- mag
     mz <- mag

     let c (Z SDSS z) (I SDSS i) = ZI SDSS (z-i)

     return $ liftA2 c mz mi
