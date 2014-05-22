{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{- |
Module      :  NiceShow
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmal.com
Stability   :  experimental
Portability :  portable


-}

module Text.NiceShow where

import Data.Point
import Text.Printf   ( printf )

class NiceShow a where
  niceShow :: a -> String

instance NiceShow Point02d where
  niceShow (a0,a1) = printf "%8.4f %8.4f" a0 a1

instance NiceShow Point03d where
  niceShow (a0,a1,a2) = printf "%8.4f %8.4f %8.4f" a0 a1 a2

instance NiceShow Point05d where
  niceShow (a0,a1,a2,a3,a4) = printf "%8.4f %8.4f %8.4f %8.4f %8.4f" a0 a1 a2 a3 a4

instance NiceShow Point07d where
  niceShow (a0,a1,a2,a3,a4,a5,a6) = printf "%8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f" a0 a1 a2 a3 a4 a5 a6

instance NiceShow Point08d where
  niceShow (a0,a1,a2,a3,a4,a5,a6,a7) = printf "%8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f" a0 a1 a2 a3 a4 a5 a6 a7 

instance NiceShow Point10d where
  niceShow (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) = printf "%8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f" a0 a1 a2 a3 a4 a5 a6 a7 a8 a9