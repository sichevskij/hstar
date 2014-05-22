{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.NumInstances
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Number class instances for functions and tuples
----------------------------------------------------------------------

module Data.NumInstances (lift2,lift3,lift4,lift5,lift7,lift8,lift9,lift10) where

import Control.Applicative

noOv :: String -> String -> a
noOv ty meth = error $ meth ++ ": No overloading for " ++ ty

noFun :: String -> a
noFun = noOv "function"

-- Eq & Show are prerequisites for Num, so they need to be faked here
instance Eq (a->b) where
  (==) = noFun "(==)"
  (/=) = noFun "(/=)"

instance Ord b => Ord (a->b) where
  min = liftA2 min
  max = liftA2 max

instance Show (a->b) where
  show      = noFun "show"
  showsPrec = noFun "showsPrec"
  showList  = noFun "showList"

instance Num b => Num (a->b) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance Fractional b => Fractional (a->b) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance Floating b => Floating (a->b) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh


----- Tuples

lift2 :: (a->u) -> (b->v) -> (a,b) -> (u,v)
lift2 f g (a,b) = (f a, g b)

-- Equivalently, lift2 = (***)

instance (Num a, Num b) => Num (a,b) where
  fromInteger n   = (fromInteger n, fromInteger n)
  (a,b) + (a',b') = (a+a',b+b')
  (a,b) - (a',b') = (a-a',b-b')
  (a,b) * (a',b') = (a*a',b*b')
  negate = lift2 negate negate
  abs    = lift2 abs abs
  signum = lift2 signum signum

instance (Fractional a, Fractional b) => Fractional (a,b) where
  fromRational x = (fromRational x, fromRational x)
  recip = lift2 recip recip

instance (Floating a, Floating b) => Floating (a,b) where
  pi    = (pi,pi)
  exp   = lift2 exp exp
  log   = lift2 log log
  sqrt  = lift2 sqrt sqrt
  sin   = lift2 sin sin
  cos   = lift2 cos cos
  sinh  = lift2 sinh sinh
  cosh  = lift2 cosh cosh
  asin  = lift2 asin asin
  acos  = lift2 acos acos
  atan  = lift2 atan atan
  asinh = lift2 asinh asinh
  acosh = lift2 acosh acosh
  atanh = lift2 atanh atanh

instance (Num a, Num b, Num c) => Num (a,b,c) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n)
  (a,b,c) + (a',b',c') = (a+a',b+b',c+c')
  (a,b,c) - (a',b',c') = (a-a',b-b',c-c')
  (a,b,c) * (a',b',c') = (a*a',b*b',c*c')
  negate = lift3 negate negate negate
  abs    = lift3 abs abs abs
  signum = lift3 signum signum signum

instance (Fractional a, Fractional b, Fractional c)
    => Fractional (a,b,c) where
  fromRational x = (fromRational x, fromRational x, fromRational x)
  recip = lift3 recip recip recip


lift3 :: (a->u) -> (b->v) -> (c->w) -> (a,b,c) -> (u,v,w)
lift3 f g h (a,b,c) = (f a, g b, h c)

instance (Floating a, Floating b, Floating c)
    => Floating (a,b,c) where
  pi    = (pi,pi,pi)
  exp   = lift3 exp exp exp
  log   = lift3 log log log
  sqrt  = lift3 sqrt sqrt sqrt
  sin   = lift3 sin sin sin
  cos   = lift3 cos cos cos
  sinh  = lift3 sinh sinh sinh
  cosh  = lift3 cosh cosh cosh
  asin  = lift3 asin asin asin
  acos  = lift3 acos acos acos
  atan  = lift3 atan atan atan
  asinh = lift3 asinh asinh asinh
  acosh = lift3 acosh acosh acosh
  atanh = lift3 atanh atanh atanh



lift4 :: (a->u) -> (b->v) -> (c->w) -> (d->x)
      -> (a,b,c,d) -> (u,v,w,x)
lift4 f g h k (a,b,c,d) = (f a, g b, h c, k d)

instance (Num a, Num b, Num c, Num d) => Num (a,b,c,d) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n)
  (a,b,c,d) + (a',b',c',d') = (a+a',b+b',c+c',d+d')
  (a,b,c,d) - (a',b',c',d') = (a-a',b-b',c-c',d-d')
  (a,b,c,d) * (a',b',c',d') = (a*a',b*b',c*c',d*d')
  negate = lift4 negate negate negate negate
  abs    = lift4 abs abs abs abs
  signum = lift4 signum signum signum signum

instance (Fractional a, Fractional b, Fractional c, Fractional d)
    => Fractional (a,b,c,d) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift4 recip recip recip recip

instance (Floating a, Floating b, Floating c, Floating d)
    => Floating (a,b,c,d) where
  pi    = (pi,pi,pi,pi)
  exp   = lift4 exp exp exp exp
  log   = lift4 log log log log
  sqrt  = lift4 sqrt sqrt sqrt sqrt
  sin   = lift4 sin sin sin sin
  cos   = lift4 cos cos cos cos
  sinh  = lift4 sinh sinh sinh sinh
  cosh  = lift4 cosh cosh cosh cosh
  asin  = lift4 asin asin asin asin
  acos  = lift4 acos acos acos acos
  atan  = lift4 atan atan atan atan
  asinh = lift4 asinh asinh asinh asinh
  acosh = lift4 acosh acosh acosh acosh
  atanh = lift4 atanh atanh atanh atanh

lift5 :: (a->u) -> (b->v) -> (c->w) -> (d->x) -> (e->y)
      -> (a,b,c,d,e) -> (u,v,w,x,y)
lift5 f g h k l (a,b,c,d,e) = (f a, g b, h c, k d, l e)

instance (Num a, Num b, Num c, Num d, Num e) => Num (a,b,c,d,e) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
  (a,b,c,d,e) + (a',b',c',d',e') = (a+a',b+b',c+c',d+d',e+e')
  (a,b,c,d,e) - (a',b',c',d',e') = (a-a',b-b',c-c',d-d',e-e')
  (a,b,c,d,e) * (a',b',c',d',e') = (a*a',b*b',c*c',d*d',e*e')
  negate = lift5 negate negate negate negate negate
  abs    = lift5 abs abs abs abs abs
  signum = lift5 signum signum signum signum signum

instance (Fractional a, Fractional b, Fractional c, Fractional d, Fractional e)
    => Fractional (a,b,c,d,e) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift5 recip recip recip recip recip

instance (Floating a, Floating b, Floating c, Floating d, Floating e)
    => Floating (a,b,c,d,e) where
  pi    = (pi,pi,pi,pi,pi)
  exp   = lift5 exp exp exp exp exp
  log   = lift5 log log log log log
  sqrt  = lift5 sqrt sqrt sqrt sqrt sqrt
  sin   = lift5 sin sin sin sin sin
  cos   = lift5 cos cos cos cos cos
  sinh  = lift5 sinh sinh sinh sinh sinh
  cosh  = lift5 cosh cosh cosh cosh cosh
  asin  = lift5 asin asin asin asin asin
  acos  = lift5 acos acos acos acos acos
  atan  = lift5 atan atan atan atan atan
  asinh = lift5 asinh asinh asinh asinh asinh
  acosh = lift5 acosh acosh acosh acosh acosh
  atanh = lift5 atanh atanh atanh atanh atanh

lift6 :: (a1->b1) -> (a2->b2) -> (a3->b3) -> (a4->b4) -> (a5->b5) -> (a6->b6)
      -> (a1,a2,a3,a4,a5,a6) -> (b1,b2,b3,b4,b5,b6)
lift6 f1 f2 f3 f4 f5 f6 (a1,a2,a3,a4,a5,a6) = (f1 a1,f2 a2,f3 a3,f4 a4,f5 a5,f6 a6)

lift7 :: (a1->b1) -> (a2->b2) -> (a3->b3) -> (a4->b4) -> (a5->b5) -> (a6->b6) -> (a7->b7)
      -> (a1,a2,a3,a4,a5,a6,a7) -> (b1,b2,b3,b4,b5,b6,b7)
lift7 f1 f2 f3 f4 f5 f6 f7 (a1,a2,a3,a4,a5,a6,a7) = (f1 a1,f2 a2,f3 a3,f4 a4,f5 a5,f6 a6,f7 a7)

lift8 :: (a1->b1) -> (a2->b2) -> (a3->b3) -> (a4->b4) -> (a5->b5) -> (a6->b6) -> (a7->b7) -> (a8->b8)
      -> (a1,a2,a3,a4,a5,a6,a7,a8) -> (b1,b2,b3,b4,b5,b6,b7,b8)
lift8 f1 f2 f3 f4 f5 f6 f7 f8 (a1,a2,a3,a4,a5,a6,a7,a8) = (f1 a1,f2 a2,f3 a3,f4 a4,f5 a5,f6 a6,f7 a7,f8 a8)

lift9 :: (a1->b1) -> (a2->b2) -> (a3->b3) -> (a4->b4) -> (a5->b5) -> (a6->b6) -> (a7->b7) -> (a8->b8) -> (a9->b9)
      -> (a1,a2,a3,a4,a5,a6,a7,a8,a9) -> (b1,b2,b3,b4,b5,b6,b7,b8,b9)
lift9 f1 f2 f3 f4 f5 f6 f7 f8 f9 (a1,a2,a3,a4,a5,a6,a7,a8,a9) = (f1 a1,f2 a2,f3 a3,f4 a4,f5 a5,f6 a6,f7 a7,f8 a8,f9 a9)

lift10 :: (a1->b1) -> (a2->b2) -> (a3->b3) -> (a4->b4) -> (a5->b5) -> (a6->b6) -> (a7->b7) -> (a8->b8) -> (a9->b9) -> (a10->b10)
      -> (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) -> (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)
lift10 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = (f1 a1,f2 a2,f3 a3,f4 a4,f5 a5,f6 a6,f7 a7,f8 a8,f9 a9,f10 a10)

instance (Num a1, Num a2, Num a3, Num a4, Num a5, Num a6, Num a7, Num a8, Num a9, Num a10) => Num (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n,fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
  (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) + (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10) = (a1+b1,a2+b2,a3+b3,a4+b4,a5+b5,a6+b6,a7+b7,a8+b8,a9+b9,a10+b10)
  (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) - (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10) = (a1-b1,a2-b2,a3-b3,a4-b4,a5-b5,a6-b6,a7-b7,a8-b8,a9-b9,a10-b10)
  (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) * (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10) = (a1*b1,a2*b2,a3*b3,a4*b4,a5*b5,a6*b6,a7*b7,a8*b8,a9*b9,a10*b10)
  negate = lift10 negate negate negate negate negate negate negate negate negate negate
  abs    = lift10 abs abs abs abs abs abs abs abs abs abs
  signum = lift10 signum signum signum signum signum signum signum signum signum signum

instance (Num a1, Num a2, Num a3, Num a4, Num a5, Num a6, Num a7, Num a8, Num a9) => Num (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n,fromInteger n, fromInteger n, fromInteger n, fromInteger n)
  (a1,a2,a3,a4,a5,a6,a7,a8,a9) + (b1,b2,b3,b4,b5,b6,b7,b8,b9) = (a1+b1,a2+b2,a3+b3,a4+b4,a5+b5,a6+b6,a7+b7,a8+b8,a9+b9)
  (a1,a2,a3,a4,a5,a6,a7,a8,a9) - (b1,b2,b3,b4,b5,b6,b7,b8,b9) = (a1-b1,a2-b2,a3-b3,a4-b4,a5-b5,a6-b6,a7-b7,a8-b8,a9-b9)
  (a1,a2,a3,a4,a5,a6,a7,a8,a9) * (b1,b2,b3,b4,b5,b6,b7,b8,b9) = (a1*b1,a2*b2,a3*b3,a4*b4,a5*b5,a6*b6,a7*b7,a8*b8,a9*b9)
  negate = lift9 negate negate negate negate negate negate negate negate negate
  abs    = lift9 abs abs abs abs abs abs abs abs abs
  signum = lift9 signum signum signum signum signum signum signum signum signum

instance (Num a1, Num a2, Num a3, Num a4, Num a5, Num a6, Num a7, Num a8) => Num (a1,a2,a3,a4,a5,a6,a7,a8) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n,fromInteger n, fromInteger n, fromInteger n)
  (a1,a2,a3,a4,a5,a6,a7,a8) + (b1,b2,b3,b4,b5,b6,b7,b8) = (a1+b1,a2+b2,a3+b3,a4+b4,a5+b5,a6+b6,a7+b7,a8+b8)
  (a1,a2,a3,a4,a5,a6,a7,a8) - (b1,b2,b3,b4,b5,b6,b7,b8) = (a1-b1,a2-b2,a3-b3,a4-b4,a5-b5,a6-b6,a7-b7,a8-b8)
  (a1,a2,a3,a4,a5,a6,a7,a8) * (b1,b2,b3,b4,b5,b6,b7,b8) = (a1*b1,a2*b2,a3*b3,a4*b4,a5*b5,a6*b6,a7*b7,a8*b8)
  negate = lift8 negate negate negate negate negate negate negate negate
  abs    = lift8 abs abs abs abs abs abs abs abs
  signum = lift8 signum signum signum signum signum signum signum signum

instance (Num a1, Num a2, Num a3, Num a4, Num a5, Num a6, Num a7) => Num (a1,a2,a3,a4,a5,a6,a7) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n,fromInteger n, fromInteger n)
  (a1,a2,a3,a4,a5,a6,a7) + (b1,b2,b3,b4,b5,b6,b7) = (a1+b1,a2+b2,a3+b3,a4+b4,a5+b5,a6+b6,a7+b7)
  (a1,a2,a3,a4,a5,a6,a7) - (b1,b2,b3,b4,b5,b6,b7) = (a1-b1,a2-b2,a3-b3,a4-b4,a5-b5,a6-b6,a7-b7)
  (a1,a2,a3,a4,a5,a6,a7) * (b1,b2,b3,b4,b5,b6,b7) = (a1*b1,a2*b2,a3*b3,a4*b4,a5*b5,a6*b6,a7*b7)
  negate = lift7 negate negate negate negate negate negate negate
  abs    = lift7 abs abs abs abs abs abs abs
  signum = lift7 signum signum signum signum signum signum signum

instance (Num a1, Num a2, Num a3, Num a4, Num a5, Num a6) => Num (a1,a2,a3,a4,a5,a6) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n,fromInteger n)
  (a1,a2,a3,a4,a5,a6) + (b1,b2,b3,b4,b5,b6) = (a1+b1,a2+b2,a3+b3,a4+b4,a5+b5,a6+b6)
  (a1,a2,a3,a4,a5,a6) - (b1,b2,b3,b4,b5,b6) = (a1-b1,a2-b2,a3-b3,a4-b4,a5-b5,a6-b6)
  (a1,a2,a3,a4,a5,a6) * (b1,b2,b3,b4,b5,b6) = (a1*b1,a2*b2,a3*b3,a4*b4,a5*b5,a6*b6)
  negate = lift6 negate negate negate negate negate negate
  abs    = lift6 abs abs abs abs abs abs
  signum = lift6 signum signum signum signum signum signum

instance ( Fractional a1, Fractional a2, Fractional a3, Fractional a4, Fractional a5
         , Fractional a6, Fractional a7, Fractional a8, Fractional a9, Fractional a10
         )
    => Fractional (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift10 recip recip recip recip recip recip recip recip recip recip

instance ( Fractional a1, Fractional a2, Fractional a3, Fractional a4, Fractional a5
         , Fractional a6, Fractional a7, Fractional a8, Fractional a9
         )
    => Fractional (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift9 recip recip recip recip recip recip recip recip recip

instance ( Fractional a1, Fractional a2, Fractional a3, Fractional a4
         , Fractional a5, Fractional a6, Fractional a7, Fractional a8
         )
    => Fractional (a1,a2,a3,a4,a5,a6,a7,a8) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift8 recip recip recip recip recip recip recip recip

instance ( Fractional a1, Fractional a2, Fractional a3, Fractional a4
         , Fractional a5, Fractional a6, Fractional a7
         )
    => Fractional (a1,a2,a3,a4,a5,a6,a7) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift7 recip recip recip recip recip recip recip

instance ( Fractional a1, Fractional a2, Fractional a3
         , Fractional a4, Fractional a5, Fractional a6
         )
    => Fractional (a1,a2,a3,a4,a5,a6) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift6 recip recip recip recip recip recip

instance ( Floating a1, Floating a2, Floating a3
         , Floating a4, Floating a5, Floating a6
         )
    => Floating (a1,a2,a3,a4,a5,a6) where
  pi    = (pi,pi,pi,pi,pi,pi)
  exp   = lift6 exp exp exp exp exp exp
  log   = lift6 log log log log log log
  sqrt  = lift6 sqrt sqrt sqrt sqrt sqrt sqrt
  sin   = lift6 sin sin sin sin sin sin
  cos   = lift6 cos cos cos cos cos cos
  sinh  = lift6 sinh sinh sinh sinh sinh sinh
  cosh  = lift6 cosh cosh cosh cosh cosh cosh
  asin  = lift6 asin asin asin asin asin asin
  acos  = lift6 acos acos acos acos acos acos
  atan  = lift6 atan atan atan atan atan atan
  asinh = lift6 asinh asinh asinh asinh asinh asinh
  acosh = lift6 acosh acosh acosh acosh acosh acosh
  atanh = lift6 atanh atanh atanh atanh atanh atanh

instance ( Floating a1, Floating a2, Floating a3, Floating a4
         , Floating a5, Floating a6, Floating a7
         )
    => Floating (a1,a2,a3,a4,a5,a6,a7) where
  pi    = (pi,pi,pi,pi,pi,pi,pi)
  exp   = lift7 exp exp exp exp exp exp exp
  log   = lift7 log log log log log log log
  sqrt  = lift7 sqrt sqrt sqrt sqrt sqrt sqrt sqrt
  sin   = lift7 sin sin sin sin sin sin sin
  cos   = lift7 cos cos cos cos cos cos cos
  sinh  = lift7 sinh sinh sinh sinh sinh sinh sinh
  cosh  = lift7 cosh cosh cosh cosh cosh cosh cosh
  asin  = lift7 asin asin asin asin asin asin asin
  acos  = lift7 acos acos acos acos acos acos acos
  atan  = lift7 atan atan atan atan atan atan atan
  asinh = lift7 asinh asinh asinh asinh asinh asinh asinh
  acosh = lift7 acosh acosh acosh acosh acosh acosh acosh
  atanh = lift7 atanh atanh atanh atanh atanh atanh atanh

instance ( Floating a1, Floating a2, Floating a3, Floating a4
         , Floating a5, Floating a6, Floating a7, Floating a8
         )
    => Floating (a1,a2,a3,a4,a5,a6,a7,a8) where
  pi    = (pi,pi,pi,pi,pi,pi,pi,pi)
  exp   = lift8 exp exp exp exp exp exp exp exp
  log   = lift8 log log log log log log log log
  sqrt  = lift8 sqrt sqrt sqrt sqrt sqrt sqrt sqrt sqrt
  sin   = lift8 sin sin sin sin sin sin sin sin
  cos   = lift8 cos cos cos cos cos cos cos cos
  sinh  = lift8 sinh sinh sinh sinh sinh sinh sinh sinh
  cosh  = lift8 cosh cosh cosh cosh cosh cosh cosh cosh
  asin  = lift8 asin asin asin asin asin asin asin asin
  acos  = lift8 acos acos acos acos acos acos acos acos
  atan  = lift8 atan atan atan atan atan atan atan atan
  asinh = lift8 asinh asinh asinh asinh asinh asinh asinh asinh
  acosh = lift8 acosh acosh acosh acosh acosh acosh acosh acosh
  atanh = lift8 atanh atanh atanh atanh atanh atanh atanh atanh

instance ( Floating a1, Floating a2, Floating a3, Floating a4, Floating a5
         , Floating a6, Floating a7, Floating a8, Floating a9
         )
    => Floating (a1,a2,a3,a4,a5,a6,a7,a8,a9) where
  pi    = (pi,pi,pi,pi,pi,pi,pi,pi,pi)
  exp   = lift9 exp exp exp exp exp exp exp exp exp
  log   = lift9 log log log log log log log log log
  sqrt  = lift9 sqrt sqrt sqrt sqrt sqrt sqrt sqrt sqrt sqrt
  sin   = lift9 sin sin sin sin sin sin sin sin sin
  cos   = lift9 cos cos cos cos cos cos cos cos cos
  sinh  = lift9 sinh sinh sinh sinh sinh sinh sinh sinh sinh
  cosh  = lift9 cosh cosh cosh cosh cosh cosh cosh cosh cosh
  asin  = lift9 asin asin asin asin asin asin asin asin asin
  acos  = lift9 acos acos acos acos acos acos acos acos acos
  atan  = lift9 atan atan atan atan atan atan atan atan atan
  asinh = lift9 asinh asinh asinh asinh asinh asinh asinh asinh asinh
  acosh = lift9 acosh acosh acosh acosh acosh acosh acosh acosh acosh
  atanh = lift9 atanh atanh atanh atanh atanh atanh atanh atanh atanh

instance ( Floating a1, Floating a2, Floating a3, Floating a4, Floating a5
         , Floating a6, Floating a7, Floating a8, Floating a9, Floating a10 
         )
    => Floating (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
  pi    = (pi,pi,pi,pi,pi,pi,pi,pi,pi,pi)
  exp   = lift10 exp exp exp exp exp exp exp exp exp exp
  log   = lift10 log log log log log log log log log log
  sqrt  = lift10 sqrt sqrt sqrt sqrt sqrt sqrt sqrt sqrt sqrt sqrt
  sin   = lift10 sin sin sin sin sin sin sin sin sin sin
  cos   = lift10 cos cos cos cos cos cos cos cos cos cos
  sinh  = lift10 sinh sinh sinh sinh sinh sinh sinh sinh sinh sinh
  cosh  = lift10 cosh cosh cosh cosh cosh cosh cosh cosh cosh cosh
  asin  = lift10 asin asin asin asin asin asin asin asin asin asin
  acos  = lift10 acos acos acos acos acos acos acos acos acos acos
  atan  = lift10 atan atan atan atan atan atan atan atan atan atan
  asinh = lift10 asinh asinh asinh asinh asinh asinh asinh asinh asinh asinh
  acosh = lift10 acosh acosh acosh acosh acosh acosh acosh acosh acosh acos
  atanh = lift10 atanh atanh atanh atanh atanh atanh atanh atanh atanh atanh
