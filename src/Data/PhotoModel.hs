{-# LANGUAGE TypeOperators, DatatypeContexts, RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
{- |
Module      :  Data.PhotoModel
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

Data type to describe the use of a photometry filter by using a certain
magnitude system configuration.  It has associated a certain zero point
object.

-}

module Data.PhotoModel
 ( PhotoModel(..)
 , UGRIZ(..)
 , JHK(..)
 , FUVNUV(..)
 , RIHα (..)
 , Lira(..)
 , Band(..)
 , Filter
 ) where


import Data.MagnitudeSystem ( MagnitudeSystem )
import Data.ZeroPoint       ( ZeroPoint, AsinhZeroPoint, PogsonZeroPoint )

-- Representation of a photometry filter as a list of points.
type Filter = [(Double,Double)]

--
data (ZeroPoint zp) => Band zp = Band
 { pfilter :: Filter
 , zeroPoint :: zp
 }

-- 
data (MagnitudeSystem ms) => PhotoModel ms bs  = PhotoModel
  { magSystem :: ms
  , bands     :: bs
  }

-- Data type to describe of SDSS bands. 
data UGRIZ = UGRIZ
  { u_band :: Band AsinhZeroPoint
  , g_band :: Band AsinhZeroPoint
  , r_band :: Band AsinhZeroPoint
  , i_band :: Band AsinhZeroPoint
  , z_band :: Band AsinhZeroPoint
  }
  
-- Data type to describe of TWOMASS bands.
data JHK = JHK
  { j_band :: Band PogsonZeroPoint
  , h_band :: Band PogsonZeroPoint
  , k_band :: Band PogsonZeroPoint
  }

-- Data type to describe of GALEX bands.
data FUVNUV = FUVNUV
  { fuv_band :: Band PogsonZeroPoint
  , nuv_band :: Band PogsonZeroPoint
  }

-- Data type to describe of IPHAS bands.
data RIHα = RIHα 
  { r'_band :: Band PogsonZeroPoint
  , i'_band :: Band PogsonZeroPoint
  , hα_band :: Band PogsonZeroPoint
  }

-- Data type to describe of Lira bands.
data Lira = Lira
  { q195_band   :: Band PogsonZeroPoint
  , q218_band   :: Band PogsonZeroPoint
  , q270_band   :: Band PogsonZeroPoint
  , q350_band   :: Band PogsonZeroPoint
  , q374c_band  :: Band PogsonZeroPoint
  , q440_band   :: Band PogsonZeroPoint
  , q550_band   :: Band PogsonZeroPoint
  , q700_band   :: Band PogsonZeroPoint
  , q785c_band  :: Band PogsonZeroPoint
  , q825_band   :: Band PogsonZeroPoint
  , q930_band   :: Band PogsonZeroPoint
  , q1000_band  :: Band PogsonZeroPoint
  , q_pn14_band :: Band PogsonZeroPoint
  , q_pn30_band :: Band PogsonZeroPoint
  }