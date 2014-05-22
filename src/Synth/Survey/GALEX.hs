{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, TypeFamilies #-}

{- |
Module      :  Synth.Survey.GALEX
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2012
License     :  BSD3

Maintainer  :  s.sichevskij@gmal.com
Stability   :  experimental
Portability :  portable

-}

module Synth.Survey.GALEX
 (
 ) where

import Control.Applicative  ( liftA2 )
import Control.Monad.Reader ( asks )
import Data.PhotoModel      ( PhotoModel( bands ), Band(..), fuv_band, nuv_band )
import Data.ZeroPoint       ( ZeroPoint( magFromFlux ) )
import Synth.Context        ( galex )
import Synth.Common         ( quad, (<*>), hc )
import Synth.Photometry     ( ModelPhotometry(..) )
import Synth.Survey


instance ModelPhotometry (FUV GALEX) where
  mag = do

     zp   <- asks $ zeroPoint . fuv_band . bands . galex
     fuv  <- asks $   pfilter . fuv_band . bands . galex

     let val f = FUV GALEX $ Just ( magFromFlux zp n )
           where
             n = (quad $ fuv <*> f)/hc

     return val

instance ModelPhotometry (NUV GALEX) where
  mag = do

     zp   <- asks $ zeroPoint.nuv_band . bands . galex
     nuv  <- asks $   pfilter.nuv_band . bands . galex

     let val f = NUV GALEX $ Just ( magFromFlux zp n )
           where
             n = (quad $ nuv <*> f)/hc

     return val

instance ModelPhotometry (FUVNUV GALEX) where
  mag = do

     mfuv <- mag
     mnuv <- mag

     let c (FUV GALEX f) (NUV GALEX n) = FUVNUV GALEX (f-n)

     return $ liftA2 c mfuv mnuv
