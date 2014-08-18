{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving, FunctionalDependencies  #-}

{- |
Module      :  Data.Object
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  not portable


-}

module Data.Object
  ( Object (..)
  , ObjDist (..)
  , GALEX(..), SDSS(..), TWOMASS(..), IPHAS(..)
  , IPHASxTWOMASS(..), SDSSxTWOMASS(..), GRIxTWOMASS(..), GRIZxTWOMASS(..)
  , NUVxSDSSxTWOMASS(..)
  )
    where

import Control.Applicative ( (<$>) )
import Control.Monad       ( replicateM )
import Data.Point          ( Point02d, Point03d, Point05d, Point06d, Point07d, Point08d, Point09d, Point10d, Point13d )
import System.Random       ( randomRIO )
import Math.Proj           ( Proj(..) )
import GHC.Float           ( double2Float, float2Double )
import GSL.Random.Gen      ( RNG )
import GSL.Random.Dist     ( getGaussian )

--
newtype GALEX              = GALEX              Point02d deriving (Show, Read, Eq, Ord, Num)
newtype       SDSS         =       SDSS         Point05d deriving (Show, Read, Eq, Ord, Num)
newtype            TWOMASS =            TWOMASS Point03d deriving (Show, Read, Eq, Ord, Num)
newtype            IPHAS   =            IPHAS   Point03d deriving (Show, Read, Eq, Ord, Num)
newtype      IPHASxTWOMASS =      IPHASxTWOMASS Point06d deriving (Show, Read, Eq, Ord, Num)
newtype       SDSSxTWOMASS =       SDSSxTWOMASS Point08d deriving (Show, Read, Eq, Ord, Num)
newtype GALEXxSDSSxTWOMASS = GALEXxSDSSxTWOMASS Point10d deriving (Show, Read, Eq, Ord, Num)
newtype        GRIxTWOMASS =        GRIxTWOMASS Point06d deriving (Show, Read, Eq, Ord, Num)
newtype       GRIZxTWOMASS =       GRIZxTWOMASS Point07d deriving (Show, Read, Eq, Ord, Num)
newtype   NUVxSDSSxTWOMASS =   NUVxSDSSxTWOMASS Point09d deriving (Show, Read, Eq, Ord, Num)


data ObjDist = Normal RNG | Uniform 

class ( Show o, Show e
      , Proj o o
      )
    => Object o e | o -> e where
  -- генерация выборки заданного объема на основе оценки параметров объекта и их точности
  genRandomObjects :: ObjDist -> Int -> (o, e) -> IO [o]


-- | GALEX
instance Proj GALEX GALEX where
  proj (GALEX p) = GALEX (proj p)

instance Proj Point10d GALEX where
  proj (x0,x1,_,_,_,_,_,_,_,_) = GALEX $ (x0,x1) - (k,k)
   where
    k = (x0+x1-1)/2

instance Proj Point13d GALEX where
  proj (x0,x1,_,_,_,_,_,_,_,_,_,_,_) = GALEX $ (x0,x1) - (k,k)
   where
    k = (x0+x1-1)/2

-- | SDSS
instance Proj SDSS SDSS where
  proj (SDSS p) = SDSS (proj p)

instance Proj Point10d SDSS where
  proj (_,_,x2,x3,x4,x5,x6,_,_,_) = SDSS $ (x2,x3,x4,x5,x6) - (k,k,k,k,k)
   where
    k = (x2+x3+x4+x5+x6-1)/5

instance Proj Point13d SDSS where
  proj (_,_,x2,x3,x4,x5,x6,_,_,_,_,_,_) = SDSS $ (x2,x3,x4,x5,x6) - (k,k,k,k,k)
   where
    k = (x2+x3+x4+x5+x6-1)/5

-- | IPHAS
instance Proj IPHAS IPHAS where
  proj (IPHAS p) = IPHAS (proj p)

instance Proj Point13d IPHAS where

  proj (_,_,_,_,_,_,_,_,_,_,x10,x11,x12) = IPHAS $ (x10,x11,x12) - (k,k,k)
   where
    k = (x10+x11+x12-1)/3

-- | 2MASS
instance Proj TWOMASS TWOMASS where
  proj (TWOMASS p) = TWOMASS (proj p)

instance Proj Point10d TWOMASS where

  proj (_,_,_,_,_,_,_,x7,x8,x9) = TWOMASS $ (x7,x8,x9) - (k,k,k)
   where
    k = (x7+x8+x9-1)/3

instance Proj Point13d TWOMASS where

  proj (_,_,_,_,_,_,_,x7,x8,x9,_,_,_) = TWOMASS $ (x7,x8,x9) - (k,k,k)
   where
    k = (x7+x8+x9-1)/3

-- | g, r, i of SDSS plus 2MASS
instance Proj GRIxTWOMASS GRIxTWOMASS where
  proj (GRIxTWOMASS (g,r,i,j,h,k) ) = GRIxTWOMASS (g',r',i', j',h',k')
    where
       (g',r',i') = proj (g,r,i)
       (j',h',k') = proj (j,h,k)

instance Proj Point10d GRIxTWOMASS where
  proj (_,_,_,x3,x4,x5,_,x7,x8,x9) = GRIxTWOMASS $ (x3,x4,x5, x7,x8,x9) - (k0,k0,k0, k1,k1,k1)
   where
    k0 = (x3+x4+x5-1)/3
    k1 = (x7+x8+x9-1)/3

instance Proj Point13d GRIxTWOMASS where
  proj (_,_,_,x3,x4,x5,_,x7,x8,x9,_,_,_) = GRIxTWOMASS $ (x3,x4,x5, x7,x8,x9) - (k0,k0,k0, k1,k1,k1)
   where
    k0 = (x3+x4+x5-1)/3
    k1 = (x7+x8+x9-1)/3

-- | g, r, i, z of SDSS and 2MASS
instance Proj GRIZxTWOMASS GRIZxTWOMASS where
  proj (GRIZxTWOMASS (g,r,i,z,j,h,k) ) = GRIZxTWOMASS (g',r',i',z', j',h',k')
    where
       (g',r',i',z') = proj (g,r,i,z)
       (j',h',k') = proj (j,h,k)

instance Proj Point10d GRIZxTWOMASS where
  proj (_,_,_,x3,x4,x5,x6,x7,x8,x9) = GRIZxTWOMASS $ (x3,x4,x5,x6, x7,x8,x9) - (k0,k0,k0,k0, k1,k1,k1)
   where
    k0 = (x3+x4+x5+x6-1)/4
    k1 = (x7+x8+x9-1)/3

instance Proj Point13d GRIZxTWOMASS where
  proj (_,_,_,x3,x4,x5,x6,x7,x8,x9,_,_,_) = GRIZxTWOMASS $ (x3,x4,x5,x6, x7,x8,x9) - (k0,k0,k0,k0, k1,k1,k1)
   where
    k0 = (x3+x4+x5+x6-1)/4
    k1 = (x7+x8+x9-1)/3

-- | IPHAS ans 2MASS
instance Proj IPHASxTWOMASS IPHASxTWOMASS where
  proj (IPHASxTWOMASS (r,i,hα,j,h,k) ) = IPHASxTWOMASS (r',i',hα', j',h',k')
    where
       (r',i',hα') = proj (r,i,hα)
       (j',h',k' ) = proj (j,h,k)


instance Proj Point13d IPHASxTWOMASS where
  proj (_,_,_,_,_,_,_,x7,x8,x9,x10,x11,x12) = IPHASxTWOMASS $ (x7,x8,x9, x10,x11,x12) - (k0,k0,k0, k1,k1,k1)
   where
    k0 = (x7  + x8  + x9  - 1)/3
    k1 = (x10 + x11 + x12 - 1)/3


-- | SDSS and 2MASS
instance Proj SDSSxTWOMASS SDSSxTWOMASS where
  proj (SDSSxTWOMASS (u,g,r,i,z,j,h,k) ) = SDSSxTWOMASS (u',g',r',i',z', j',h',k')
    where
       (u',g',r',i',z') = proj (u,g,r,i,z)
       (j',h',k') = proj (j,h,k)

instance Proj Point10d SDSSxTWOMASS where
  proj (_,_,x2,x3,x4,x5,x6,x7,x8,x9) = SDSSxTWOMASS $ (x2,x3,x4,x5,x6, x7,x8,x9) - (k0,k0,k0,k0,k0, k1,k1,k1)
   where
    k0 = (x2+x3+x4+x5+x6-1)/5
    k1 = (x7+x8+x9-1)/3

instance Proj Point13d SDSSxTWOMASS where
  proj (_,_,x2,x3,x4,x5,x6,x7,x8,x9,_,_,_) = SDSSxTWOMASS $ (x2,x3,x4,x5,x6, x7,x8,x9) - (k0,k0,k0,k0,k0, k1,k1,k1)
   where
    k0 = (x2+x3+x4+x5+x6-1)/5
    k1 = (x7+x8+x9-1)/3

-- | NUV + SDSS + 2MASS
instance Proj NUVxSDSSxTWOMASS NUVxSDSSxTWOMASS where
  proj (NUVxSDSSxTWOMASS (nuv, u,g,r,i,z,j,h,k) ) = NUVxSDSSxTWOMASS (nuv-u, u',g',r',i',z', j',h',k')
    where
       (u',g',r',i',z') = proj (u,g,r,i,z)
       (j',h',k') = proj (j,h,k)

instance Proj Point10d NUVxSDSSxTWOMASS where
  proj (_,x1,x2,x3,x4,x5,x6,x7,x8,x9) = NUVxSDSSxTWOMASS $ (x1, x2,x3,x4,x5,x6, x7,x8,x9) - (x2, k0,k0,k0,k0,k0, k1,k1,k1)
   where
    k0 = (x2+x3+x4+x5+x6-1)/5
    k1 = (x7+x8+x9-1)/3

instance Proj Point13d NUVxSDSSxTWOMASS where
  proj (_,x1,x2,x3,x4,x5,x6,x7,x8,x9,_,_,_) = NUVxSDSSxTWOMASS $ (x1, x2,x3,x4,x5,x6, x7,x8,x9) - (x2, k0,k0,k0,k0,k0, k1,k1,k1)
   where
    k0 = (x2+x3+x4+x5+x6-1)/5
    k1 = (x7+x8+x9-1)/3

instance Object GALEX Point02d where
  genRandomObjects od n (o,e) = do

    let (e0,e1) = e

    es <- replicateM n $ do 
      x0 <- getRandom od e0
      x1 <- getRandom od e1
      return $ GALEX (x0,x1)

    return $ (o+) <$> es

instance Object SDSS Point05d where
  genRandomObjects od n (o,e) = do

    let (e0,e1,e2,e3,e4) = e

    es <- replicateM n $ do 
      x0 <- getRandom od e0
      x1 <- getRandom od e1
      x2 <- getRandom od e2
      x3 <- getRandom od e3
      x4 <- getRandom od e4
      return $ SDSS (x0,x1,x2,x3,x4)

    return $ (o+) <$> es

instance Object IPHAS Point03d where
  genRandomObjects od n (o,e) = do

    let (e0,e1,e2) = e

    es <- replicateM n $ do 
      x0 <- getRandom od e0
      x1 <- getRandom od e1
      x2 <- getRandom od e2
      return $ IPHAS (x0,x1,x2)

    return $ (o+) <$> es

instance Object TWOMASS Point03d where
  genRandomObjects od n (o,e) = do

    let (e0,e1,e2) = e

    es <- replicateM n $ do 
      x0 <- getRandom od e0
      x1 <- getRandom od e1
      x2 <- getRandom od e2
      return $ TWOMASS (x0,x1,x2)

    return $ (o+) <$> es

instance Object IPHASxTWOMASS Point06d where
  genRandomObjects od n (o,e) = do

    let (e0,e1,e2,e3,e4,e5) = e

    es <- replicateM n $ do 
      x0 <- getRandom od e0
      x1 <- getRandom od e1
      x2 <- getRandom od e2
      x3 <- getRandom od e3
      x4 <- getRandom od e4
      x5 <- getRandom od e5
      return $ IPHASxTWOMASS (x0,x1,x2,x3,x4,x5)

    return $ (o+) <$> es

instance Object SDSSxTWOMASS Point08d where
  genRandomObjects od n (o,e) = do

    let (e0,e1,e2,e3,e4,e5,e6,e7) = e

    es <- replicateM n $ do 
      x0 <- getRandom od e0
      x1 <- getRandom od e1
      x2 <- getRandom od e2
      x3 <- getRandom od e3
      x4 <- getRandom od e4
      x5 <- getRandom od e5
      x6 <- getRandom od e6
      x7 <- getRandom od e7
      return $ SDSSxTWOMASS (x0,x1,x2,x3,x4,x5,x6,x7)

    return $ (o+) <$> es

instance Object GRIxTWOMASS Point06d where
  genRandomObjects od n (o,e) = do

    let (e0,e1,e2,e3,e4,e5) = e

    es <- replicateM n $ do 
      x0 <- getRandom od e0
      x1 <- getRandom od e1
      x2 <- getRandom od e2
      x3 <- getRandom od e3
      x4 <- getRandom od e4
      x5 <- getRandom od e5
      return $ GRIxTWOMASS (x0,x1,x2,x3,x4,x5)

    return $ (o+) <$> es

instance Object GRIZxTWOMASS Point07d where
  genRandomObjects od n (o,e) = do

    let (e0,e1,e2,e3,e4,e5,e6) = e

    es <- replicateM n $ do 
      x0 <- getRandom od e0
      x1 <- getRandom od e1
      x2 <- getRandom od e2
      x3 <- getRandom od e3
      x4 <- getRandom od e4
      x5 <- getRandom od e5
      x6 <- getRandom od e6
      return $ GRIZxTWOMASS (x0,x1,x2,x3,x4,x5,x6)

    return $ (o+) <$> es



instance Object NUVxSDSSxTWOMASS Point09d where
  genRandomObjects od n (o,e) = do

    let (e0,e1,e2,e3,e4,e5,e6,e7,e8) = e

    es <- replicateM n $ do 
      x0 <- getRandom od e0
      x1 <- getRandom od e1
      x2 <- getRandom od e2
      x3 <- getRandom od e3
      x4 <- getRandom od e4
      x5 <- getRandom od e5
      x6 <- getRandom od e6
      x7 <- getRandom od e7
      x8 <- getRandom od e8
      return $ NUVxSDSSxTWOMASS (x0,x1,x2,x3,x4,x5,x6,x7,x8)

    return $ (o+) <$> es

getRandom :: ObjDist -> Float -> IO Float
getRandom (Normal rng) e = do
  x <- getGaussian rng (float2Double e)
  return (double2Float x)
getRandom Uniform e = randomRIO (-e,e)

