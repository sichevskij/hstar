{-# LANGUAGE OverlappingInstances, FlexibleContexts, TypeOperators, MultiParamTypeClasses, UndecidableInstances, FlexibleInstances, DeriveDataTypeable, TypeFamilies, FunctionalDependencies #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.HList
-- Copyright   :  (c) Sergey Sichevskiy 2012
-- License     :  BSD3
-- 
-- Maintainer  :  s.sichevskij@gmal.com
-- Stability   :  experimental
--  
-- Основные определения для неоднородного списка
--
----------------------------------------------------------------------

module Data.HList where

import Prelude (Eq,Ord,Show,Read,Int,Double,undefined,($),error)

import Data.Typeable
import Data.TypeLevel.Num
import Data.TypeLevel.Bool

--

data         ToDouble =   ToDouble
data       FromDouble = FromDouble

type instance Applay   ToDouble a = Double
type instance Applay FromDouble (a,Double) = a


-- Последовательность разнородных типов

data  Null      = Null     deriving (Typeable,Eq,Show,Read,Ord)
data (:::) x xs = x ::: xs deriving (Typeable,Eq,Show,Read,Ord)

infixr 5 :::

-- Класс HList

class HList x
instance HList Null
instance HList xs => HList (x:::xs)

-- HLength

type family   Length l
type instance Length Null = D0
type instance Length (x ::: xs) = Add D1 (Length xs)

type family Head l
type instance Head (x ::: xs) = x

type family Tail l
type instance Tail (x ::: xs) = xs

{--
type family Reverse l
type instance Reverse l = Reverse' l Null

type family Reverse' l a
type instance Reverse' Null a = a
type instance Reverse' (x ::: xs) a = Reverse' xs (x ::: a)
--}

hHead :: HList xs => x:::xs -> x
hHead (x:::_)= x

hTail :: HList xs => x:::xs -> xs
hTail (_:::xs)= xs

hLength :: (Nat (Length xs), HList xs) => xs -> Length xs
hLength _ = undefined

-- Класс HAppend

type family   Append l1   l2
type instance Append Null l2 = l2
type instance Append (x ::: xs2) l2 =  x ::: (Append xs2 l2)

class (HList xs, HList ys) => HAppend xs ys where
  hAppend :: xs -> ys -> Append xs ys

instance (HList ys) => HAppend Null ys where
  hAppend _ ys = ys

instance (HList ys, HAppend xs ys) => HAppend (x:::xs) ys where
  hAppend (x:::xs) ys = x ::: (xs `hAppend` ys)



-- * Splitting by HTrue and HFalse

-- | Analogus to @Data.List.partition snd@

type family   Split l 
type instance Split Null     = (Null, Null)
type instance Split (x:::xs) = (SplitTrue (x:::xs), SplitFalse (x:::xs))

type family   SplitTrue l 
type instance SplitTrue Null = Null
type instance SplitTrue ((x,False) ::: xs) = SplitTrue xs
type instance SplitTrue ((x, True) ::: xs) = Append (x:::Null) (SplitTrue xs)

type family   SplitFalse l 
type instance SplitFalse Null = Null
type instance SplitFalse ((x,False) ::: xs) = Append (x:::Null) (SplitFalse xs)
type instance SplitFalse ((x, True) ::: xs) = SplitFalse xs

class HSplit xs where
  hSplit :: xs -> Split xs

instance HSplit Null where 
  hSplit _ = (Null,Null)

instance ( HSplit xs
         , Split xs ~ (SplitTrue xs, SplitFalse xs)
         ) => HSplit ((x,True):::xs)
           where
  hSplit ( (x,_):::xs) = (x:::l',l'') where (l',l'') = hSplit xs

instance ( HSplit xs
         , Split xs ~ (SplitTrue xs, SplitFalse xs)
         ) => HSplit ((x,False):::xs)
           where
  hSplit ( (x,_):::xs) = (l', x:::l'') where (l',l'') = hSplit xs

-- Класс HFlag

type family   Flag i n xs
type instance Flag i n Null = Null
type instance Flag i n (x:::xs) =  (x, i :<: n) ::: (Flag (Succ i) n xs)

class HFlag i n xs where
 hFlag :: i -> n -> xs -> Flag i n xs

instance  HFlag i n Null where
 hFlag _ _ _ = Null

instance (HFlag (Succ i) n xs) => HFlag i n (x:::xs) where
 hFlag i n (x:::xs) = (x, i < n ) ::: (hFlag (succ i) n xs)

-- Класс HSplitAt

type family   SplitAt n l
type instance SplitAt n l = Split (Flag D0 n l)

class ( HFlag D0 n xy
      , HSplit (Flag D0 n xy)
      )
      => HSplitAt n xy where
  hSplitAt' :: n -> xy -> SplitAt n xy
  hSplitAt' n xy = hSplit $ hFlag d0 n xy

instance ( HFlag D0 n xy, HSplit (Flag D0 n xy) ) => HSplitAt n xy

hSplitAt :: (HSplitAt n xy) => n -> xy -> SplitAt n xy
hSplitAt = hSplitAt'

--- Класс HSplitAt2

class HSplitAt2 xs ys where
  hSplitAt2 :: Length xs -> Append xs ys -> (xs,ys)

instance HSplitAt2 Null Null where
  hSplitAt2 _ _ = (Null,Null)

instance HSplitAt2 Null ys where
  hSplitAt2 _ xy = (Null,xy)

instance ( HSplitAt2 xs ys
         , n ~ Length xs
         , n ~ Pred (Succ n)
         , n' ~ Length ys
         , Add n n' ~ Length (Append xs ys)
         )
      =>   HSplitAt2 (x:::xs) ys
 where
  hSplitAt2 n (x:::xs) = (x:::l',l'' )
     where
      (l',l'') = hSplitAt2 (pred n) xs

-- Класс HApplay   

type family   Applay f a
type instance Applay (a->b) a = b

class HApplay f a where
  applay :: f -> a -> Applay f a

instance HApplay (a->b) a where
  applay f x = f x

-- Класс HMap

type family   Map f xs
type instance Map f Null = Null
type instance Map f (x:::xs) = (Applay f x) ::: (Map f xs)

class (HList xs) => HMap f xs where
  hMap :: f -> xs -> Map f xs

instance HMap f Null where
  hMap _ _ = Null

instance (HMap f xs, HApplay f x) => HMap f (x:::xs) where
  hMap f (x:::xs) = (f `applay` x) ::: hMap f xs

-- Класс HFoldr

type family   Foldr f v xs
type instance Foldr f v Null = v
type instance Foldr f v (x:::xs) = Applay f (x,Foldr f v xs)

class (HList xs) => HFoldr f v xs where
  hFoldr :: f -> v -> xs -> Foldr f v xs

instance HFoldr f v Null where
  hFoldr _ v _ = v

instance ( HFoldr f v xs
         , HApplay f (x, Foldr f v xs)
         )
      =>   HFoldr f v (x:::xs) where
  hFoldr f v (x:::xs) = applay f (x, hFoldr f v xs)

-- Turn a heterogeneous list into a homogeneous one

class HToList l e where
  hToList :: l -> [e]

instance HToList Null e  where
  hToList Null = []

instance HToList l e => HToList (e ::: l) e where
  hToList (e ::: l) = e : hToList l

  -- HFromList

class HList l => HFromList e l where
  hFromList :: [e] -> l

instance HFromList e Null where
  hFromList _ = Null

instance HFromList e l => HFromList e (e ::: l) where
  hFromList    [] = error "hFromList xs: xs does not have the right length "
  hFromList (e:l) = e ::: (hFromList l)

 -- Класс HZip

--{--
type family   Zip xs ys
type instance Zip Null ys = Null
type instance Zip ys Null = Null
type instance Zip (x:::xs) (y:::ys) = (x,y) ::: Zip xs ys

class (HList xs, HList ys, HList (Zip xs ys)) => HZip xs ys where
  hZip :: xs -> ys -> Zip xs ys

instance HList xs => HZip xs Null where
  hZip _ _ = Null

instance HList ys => HZip Null ys where
  hZip _ _ = Null

instance HZip Null Null where
  hZip _ _ = Null

instance HZip xs ys => HZip (x:::xs) (y:::ys) where
  hZip (x:::xs) (y:::ys) = (x,y) ::: hZip xs ys
--}

{--
class HZip x y l | x y -> l, l -> x y
 where
  hZip   :: x -> y -> l

instance HZip Null Null Null
 where
  hZip Null Null = Null

instance HZip tx ty l
      => HZip (hx ::: tx) (hy ::: ty) ((hx,hy) ::: l)
 where
  hZip (hx ::: tx) (hy ::: ty) = (hx,hy) ::: (hZip tx ty)
--}

-- HBetween

class (HList l) => HBetween n l | l -> n, n -> l where
  hBetween :: n -> l

instance HBetween D0 (Int:::Null) where
  hBetween n = (toInt n) ::: Null

instance ( Pos n, HBetween (Pred n) xs) => HBetween n (Int:::xs) where
  hBetween n = (toInt n) ::: (hBetween (pred n))
