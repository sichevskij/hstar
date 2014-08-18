{- |
Module      :  Server.Request
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Server.Request 
 ( Request(..)
 , Cmd(..)
 , Object(..)
 )
   where

import Data.Point 
import Data.Object ( GALEX(..), SDSS(..), TWOMASS(..), IPHAS(..), SDSSxTWOMASS(..), IPHASxTWOMASS(..), GRIxTWOMASS(..), GRIZxTWOMASS(..), NUVxSDSSxTWOMASS(..) )


data Object
  = OxGALEX            GALEX		Point02d
  | OxSDSS             SDSS		Point05d
  | OxTWOMASS          TWOMASS		Point03d
  | OxIPHAS            IPHAS		Point03d
  | OxGRIxTWOMASS      GRIxTWOMASS	Point06d
  | OxGRIZxTWOMASS     GRIZxTWOMASS	Point07d
  | OxIPHASxTWOMASS    IPHASxTWOMASS	Point06d
  | OxSDSSxTWOMASS     SDSSxTWOMASS	Point08d
  | OxNUVxSDSSxTWOMASS NUVxSDSSxTWOMASS	Point09d
  deriving (Show, Read)

data Cmd 
  = Echo String
  | Task Object
  | Estimate Object 
  deriving (Show, Read)

data Request = Req Cmd deriving (Show, Read)