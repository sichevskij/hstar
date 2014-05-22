{-# LANGUAGE TypeOperators #-}
----------------------------------------------------------------------
-- |
-- Module      :   Data.AdditiveGroup
-- Copyright   :  (c) Conal Elliott and Andy J Gill 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net, andygill@ku.edu
-- Stability   :  experimental
-- 
-- Groups: zero, addition, and negation (additive inverse)
----------------------------------------------------------------------

module Data.AdditiveGroup
  ( 
    AdditiveGroup(..), (^-^), sumV
  , Sum(..), inSum, inSum2
  ) where

import Control.Applicative
import Data.Monoid (Monoid(..))
import Data.Complex hiding (magnitude)

--import Data.MemoTrie

infixl 6 ^+^, ^-^

-- | Additive group @v@.
class AdditiveGroup v where
  -- | The zero element: identity for '(^+^)'
  zeroV :: v
  -- | Add vectors
  (^+^) :: v -> v -> v
  -- | Additive inverse
  negateV :: v -> v

-- | Group subtraction
(^-^) :: AdditiveGroup v => v -> v -> v
v ^-^ v' = v ^+^ negateV v'

-- | Sum over several vectors
sumV :: AdditiveGroup v => [v] -> v
sumV = foldr (^+^) zeroV


instance AdditiveGroup () where
  zeroV     = ()
  () ^+^ () = ()
  negateV   = id

-- For 'Num' types:
-- 
-- instance AdditiveGroup n where {zeroV=0; (^+^) = (+); negateV = negate}

instance AdditiveGroup Int     where {zeroV=0; (^+^) = (+); negateV = negate}
instance AdditiveGroup Integer where {zeroV=0; (^+^) = (+); negateV = negate}
instance AdditiveGroup Float   where {zeroV=0; (^+^) = (+); negateV = negate}
instance AdditiveGroup Double  where {zeroV=0; (^+^) = (+); negateV = negate}


instance (RealFloat v, AdditiveGroup v) => AdditiveGroup (Complex v) where
  zeroV   = zeroV :+ zeroV
  (^+^)   = (+)
  negateV = negate

-- Hm.  The 'RealFloat' constraint is unfortunate here.  It's due to a
-- questionable decision to place 'RealFloat' into the definition of the
-- 'Complex' /type/, rather than in functions and instances as needed.

instance (AdditiveGroup u,AdditiveGroup v) => AdditiveGroup (u,v) where
  zeroV             = (zeroV,zeroV)
  (u,v) ^+^ (u',v') = (u^+^u',v^+^v')
  negateV (u,v)     = (negateV u,negateV v)

instance (AdditiveGroup u,AdditiveGroup v,AdditiveGroup w)
    => AdditiveGroup (u,v,w) where
  zeroV                  = (zeroV,zeroV,zeroV)
  (u,v,w) ^+^ (u',v',w') = (u^+^u',v^+^v',w^+^w')
  negateV (u,v,w)        = (negateV u,negateV v,negateV w)

instance (AdditiveGroup u,AdditiveGroup v,AdditiveGroup w,AdditiveGroup x)
    => AdditiveGroup (u,v,w,x) where
  zeroV                       = (zeroV,zeroV,zeroV,zeroV)
  (u,v,w,x) ^+^ (u',v',w',x') = (u^+^u',v^+^v',w^+^w',x^+^x')
  negateV (u,v,w,x)           = (negateV u,negateV v,negateV w,negateV x)

instance ( AdditiveGroup a1
         , AdditiveGroup a2
         , AdditiveGroup a3
         , AdditiveGroup a4
         , AdditiveGroup a5 )
    => AdditiveGroup (a1,a2,a3,a4,a5) where
  zeroV                                 = (zeroV,zeroV,zeroV,zeroV,zeroV)
  (a1,a2,a3,a4,a5) ^+^ (b1,b2,b3,b4,b5) = (a1^+^b1,a2^+^b2,a3^+^b3,a4^+^b4,a5^+^b5)
  negateV (a1,a2,a3,a4,a5)              = (negateV a1,negateV a2,negateV a3,negateV a4,negateV a5)

instance ( AdditiveGroup a1
         , AdditiveGroup a2
         , AdditiveGroup a3
         , AdditiveGroup a4
         , AdditiveGroup a5
         , AdditiveGroup a6
         , AdditiveGroup a7
         , AdditiveGroup a8 )
    => AdditiveGroup (a1,a2,a3,a4,a5,a6,a7,a8) where
  zeroV                                                   = (zeroV,zeroV,zeroV,zeroV,zeroV,zeroV,zeroV,zeroV)
  (a1,a2,a3,a4,a5,a6,a7,a8) ^+^ (b1,b2,b3,b4,b5,b6,b7,b8) = (a1^+^b1,a2^+^b2,a3^+^b3,a4^+^b4,a5^+^b5,a6^+^b6,a7^+^b7,a8^+^b8)
  negateV (a1,a2,a3,a4,a5,a6,a7,a8)                       = (negateV a1,negateV a2,negateV a3,negateV a4,negateV a5,negateV a6,negateV a7,negateV a8)

instance ( AdditiveGroup a1
         , AdditiveGroup a2
         , AdditiveGroup a3
         , AdditiveGroup a4
         , AdditiveGroup a5
         , AdditiveGroup a6
         , AdditiveGroup a7
         , AdditiveGroup a8
         , AdditiveGroup a9
         , AdditiveGroup a10 )
    => AdditiveGroup (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where
  zeroV                                                                 = (zeroV,zeroV,zeroV,zeroV,zeroV,zeroV,zeroV,zeroV,zeroV,zeroV)
  (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) ^+^ (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10) = (a1^+^b1,a2^+^b2,a3^+^b3,a4^+^b4,a5^+^b5,a6^+^b6,a7^+^b7,a8^+^b8,a9^+^b9,a10^+^b10)
  negateV (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)                              = (negateV a1,negateV a2,negateV a3,negateV a4,negateV a5,negateV a6,negateV a7,negateV a8,negateV a9,negateV a10)


-- Standard instance for an applicative functor applied to a vector space.
instance AdditiveGroup v => AdditiveGroup (a -> v) where
  zeroV   = pure   zeroV
  (^+^)   = liftA2 (^+^)
  negateV = fmap   negateV


-- Maybe is handled like the Maybe-of-Sum monoid
instance AdditiveGroup a => AdditiveGroup (Maybe a) where
  zeroV = Nothing
  Nothing ^+^ b'      = b'
  a' ^+^ Nothing      = a'
  Just a' ^+^ Just b' = Just (a' ^+^ b')
  negateV = fmap negateV


-- Memo tries
--instance (HasTrie u, AdditiveGroup v) => AdditiveGroup (u :->: v) where
--  zeroV   = pure   zeroV
--  (^+^)   = liftA2 (^+^)
--  negateV = fmap   negateV


-- | Monoid under group addition.  Alternative to the @Sum@ in
-- "Data.Monoid", which uses 'Num' instead of 'AdditiveGroup'.
newtype Sum a = Sum { getSum :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Functor Sum where
  fmap f (Sum a) = Sum (f a)

-- instance Applicative Sum where
--   pure a = Sum a
--   Sum f <*> Sum x = Sum (f x)

instance Applicative Sum where
  pure  = Sum
  (<*>) = inSum2 ($)

instance AdditiveGroup a => Monoid (Sum a) where
  mempty  = Sum zeroV
  mappend = liftA2 (^+^)


-- | Application a unary function inside a 'Sum'
inSum :: (a -> b) -> (Sum a -> Sum b)
inSum = getSum ~> Sum

-- | Application a binary function inside a 'Sum'
inSum2 :: (a -> b -> c) -> (Sum a -> Sum b -> Sum c)
inSum2 = getSum ~> inSum


instance AdditiveGroup a => AdditiveGroup (Sum a) where
  zeroV   = mempty
  (^+^)   = mappend
  negateV = inSum negateV


---- to go elsewhere

(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
(i ~> o) f = o . f . i

-- result :: (b -> b') -> ((a -> b) -> (a -> b'))
-- result = (.)

-- argument :: (a' -> a) -> ((a -> b) -> (a' -> b))
-- argument = flip (.)

-- g ~> f = result g . argument f
