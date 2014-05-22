{-# LANGUAGE CPP, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveFunctor, OverlappingInstances #-}
{- |
Module      :  Synth.Survey
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}


module Synth.Survey where

import Control.Applicative ( pure, liftA2 )
import Data.HList          ( HApplay(..), ToDouble(..) )
import Text.Printf         ( printf )
import Text.NiceShow       ( NiceShow(..) )
import Text.Read

import qualified Text.Read.Lex as L


type Val = Maybe Double --deriving (Show, Num)

{--
type Val = Maybe ( Double -- value
                 , Double -- uncertainty
                 )
--}

class Survey s where

data GALEX = GALEX deriving (Show,Read)
data SDSS = SDSS deriving (Show,Read)
data TWOMASS = TWOMASS deriving (Show,Read)
data DENIS = DENIS deriving (Show,Read)
data LIRA = LIRA deriving (Show,Read)
data WBVR = WBVR deriving (Show,Read)

-- Блески
data I s = I s Val deriving (Show,Functor,Read)
data J s = J s Val deriving (Show,Functor)
data H s = H s Val deriving (Show,Functor)
data K s = K s Val deriving (Show,Functor)
data U s = U s Val deriving (Show,Functor,Read)
data G s = G s Val deriving (Show,Functor,Read)
data Z s = Z s Val deriving (Show,Functor,Read)
data W s = W s Val deriving (Show,Functor)
data B s = B s Val deriving (Show,Functor)
data V s = V s Val deriving (Show,Functor)
data R s = R s Val deriving (Show,Functor,Read)
data NUV s = NUV s Val deriving (Show,Functor)
data FUV s = FUV s Val deriving (Show,Functor,Read)

data Q195 s = Q195 s Val deriving (Show,Functor)
data Q218 s = Q218 s Val deriving (Show,Functor)
data Q270 s = Q270 s Val deriving (Show,Functor)
data Q350 s = Q350 s Val deriving (Show,Functor)
data Q374C s = Q374C s Val deriving (Show,Functor)
data Q440 s = Q440 s Val deriving (Show,Functor)
data Q550 s = Q550 s Val deriving (Show,Functor)
data Q700 s = Q700 s Val deriving (Show,Functor)
data Q785C s = Q785C s Val deriving (Show,Functor)
data Q825 s = Q825 s Val deriving (Show,Functor)
data Q930 s = Q930 s Val deriving (Show,Functor)
data Q1000 s = Q1000 s Val deriving (Show,Functor)
data QPN14 s = QPN14 s Val deriving (Show,Functor)
data QPN30 s = QPN30 s Val deriving (Show,Functor)

-- Цвета
data IJ s = IJ s Val deriving (Show,Functor)
data IK s = IK s Val deriving (Show,Functor)
data JH s = JH s Val deriving (Show,Functor)
data JK s = JK s Val deriving (Show,Functor)
data HK s = HK s Val deriving (Show,Functor)

data GU s = GU s Val deriving (Show,Functor)
data RU s = RU s Val deriving (Show,Functor)
data IU s = IU s Val deriving (Show,Functor)
data ZU s = ZU s Val deriving (Show,Functor)
data RG s = RG s Val deriving (Show,Functor)
data IG s = IG s Val deriving (Show,Functor)
data ZG s = ZG s Val deriving (Show,Functor)
data IR s = IR s Val deriving (Show,Functor)
data ZR s = ZR s Val deriving (Show,Functor)
data ZI s = ZI s Val deriving (Show,Functor)

data WB s = WB s Val deriving (Show,Functor)
data WV s = WV s Val deriving (Show,Functor)
data WR s = WR s Val deriving (Show,Functor)
data BV s = BV s Val deriving (Show,Functor)
data BR s = BR s Val deriving (Show,Functor)
data VR s = VR s Val deriving (Show,Functor)

data FUVNUV s = FUVNUV s Val deriving (Show,Functor)
--{--
instance Read (Maybe Double) where
  readPrec =
    parens
    (do L.Ident "Nothing" <- lexP
        return Nothing
     +++
     prec 0 (
        do L.Ident "Just" <- lexP
           x              <- step readPrec
           return (Just x))
     +++
     prec 0 (
        do --L.Ident "Just" <- lexP
           x              <- step readPrec
           return (Just x))
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Num Val where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance Fractional Val where
  fromRational  = pure . fromRational
  recip = fmap recip
--}

{-

 ТОDO: Здесь смысловая ошибка! Nothing == Just 0, что может привести к проблемам.

-}
--{--
#define NUMERIC(T)                                                      \
instance HApplay ToDouble (T s) where { applay _ (T _ v) = maybe 0 id v }

NUMERIC (Q195 )
NUMERIC (Q218 )
NUMERIC (Q270 )
NUMERIC (Q350 )
NUMERIC (Q374C )
NUMERIC (Q440 )
NUMERIC (Q550 )
NUMERIC (Q700 )
NUMERIC (Q785C )
NUMERIC (Q825 )
NUMERIC (Q930 )
NUMERIC (Q1000 )
NUMERIC (QPN14 )
NUMERIC (QPN30 )

NUMERIC (I )
NUMERIC (J )
NUMERIC (H )
NUMERIC (K )

NUMERIC (U )
NUMERIC (G )
NUMERIC (Z )
NUMERIC (W )
NUMERIC (B )
NUMERIC (V )
NUMERIC (R )

NUMERIC (NUV)
NUMERIC (FUV)

NUMERIC (IJ)
NUMERIC (IK)
NUMERIC (JH)
NUMERIC (JK)
NUMERIC (HK)

NUMERIC (GU)
NUMERIC (RU)
NUMERIC (IU)
NUMERIC (ZU)
NUMERIC (RG)
NUMERIC (IG)
NUMERIC (ZG)
NUMERIC (IR)
NUMERIC (ZR)
NUMERIC (ZI)

NUMERIC (WB)
NUMERIC (WV)
NUMERIC (WR)
NUMERIC (BV)
NUMERIC (BR)
NUMERIC (VR)

NUMERIC (FUVNUV)

--}


#define NUM(T, S)                            \
instance Num (T S) where                     \
  negate (T S a)      = T S $ negate a      ;\
  (+) (T S a) (T _ b) = T S $ a+b           ;\
  (*) (T S a) (T _ b) = T S $ a*b           ;\
  fromInteger a       = T (undefined::S) $ fromInteger a ;\
  abs (T S a)         = T S $ abs a         ;\
  signum (T S a)      = T S $ signum a

NUM (V,  s)
NUM (IJ, s)
NUM (IK, s)
NUM (JH, s)
NUM (JK, s)
NUM (HK, s)
NUM (WB, s)
NUM (WV, s)
NUM (WR, s)
NUM (BV, s)
NUM (BR, s)
NUM (VR, s)

#define NICESHOW(TA, TB)                                                          \
instance Show TB => NiceShow (TA TB)                                              \
   where                                                                          \
      niceShow (TA TB (Just v)) = printf "%s %3s %10.7f"(show TB) "TA" v         ;\
      niceShow (TA TB  Nothing) = printf "%s %3s %10s"  (show TB) "TA" "Nothing"

NICESHOW (I, s)
NICESHOW (J, s)
NICESHOW (H, s)
NICESHOW (K, s)
NICESHOW (U, s)
NICESHOW (G, s)
NICESHOW (Z, s)
NICESHOW (W, s)
NICESHOW (B, s)
NICESHOW (V, s)
NICESHOW (R, s)

NICESHOW (NUV, s)
NICESHOW (FUV, s)

NICESHOW (IJ, s)
NICESHOW (IK, s)
NICESHOW (JH, s)
NICESHOW (JK, s)
NICESHOW (HK, s)

NICESHOW (GU, s)
NICESHOW (RU, s)
NICESHOW (IU, s)
NICESHOW (ZU, s)
NICESHOW (RG, s)
NICESHOW (IG, s)
NICESHOW (ZG, s)
NICESHOW (IR, s)
NICESHOW (ZR, s)
NICESHOW (ZI, s)

NICESHOW (WB, s)
NICESHOW (WV, s)
NICESHOW (WR, s)
NICESHOW (BV, s)
NICESHOW (BR, s)
NICESHOW (VR, s)

NICESHOW (FUVNUV, s)