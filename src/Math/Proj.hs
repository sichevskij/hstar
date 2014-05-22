{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module Math.Proj
  ( Proj(..)
  )
    where

import Data.Point

class (Num a, Num b) => Proj a b where
  proj :: a -> b


instance Proj Point02d Point02d where
  proj (x0,x1) = (x0,x1) - (k,k)
   where
    k = (x0+x1-1)/2

instance Proj Point03d Point03d where
  proj (x0,x1,x2) = (x0,x1,x2) - (k,k,k)
   where
    k = (x0+x1+x2-1)/3

instance Proj Point04d Point04d where
  proj (x0,x1,x2,x3) = (x0,x1,x2,x3) - (k,k,k,k)
   where
    k = (x0+x1+x2+x3-1)/4


instance Proj Point05d Point05d where
  proj (x0,x1,x2,x3,x4) = (x0,x1,x2,x3,x4) - (k,k,k,k,k)
   where
    k = (x0+x1+x2+x3+x4-1)/5

instance Proj Point06d Point06d where
  proj (x0,x1,x2,x3,x4,x5) = (x0,x1,x2,x3,x4,x5) - (k,k,k,k,k,k)
   where
    k = (x0+x1+x2+x3+x4+x5-1)/6

instance Proj Point08d Point08d where
  proj (x0,x1,x2,x3,x4,x5,x6,x7) = (x0,x1,x2,x3,x4,x5,x6,x7) - (k,k,k,k,k,k,k,k)
   where
    k = (x0+x1+x2+x3+x4+x5+x6+x7-1)/8
