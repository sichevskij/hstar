{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

{- |
Module      :  Data.Point
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable


-}

module Data.Point where

import Data.NumInstances ()
import Foreign           ( Storable(..) )
import Foreign.C.Types   ( CFloat(..) )

type Point02d = (Float, Float)
type Point03d = (Float, Float, Float)
type Point04d = (Float, Float, Float, Float)
type Point05d = (Float, Float, Float, Float, Float)
type Point06d = (Float, Float, Float, Float, Float, Float)
type Point07d = (Float, Float, Float, Float, Float, Float, Float)
type Point08d = (Float, Float, Float, Float, Float, Float, Float, Float)
type Point09d = (Float, Float, Float, Float, Float, Float, Float, Float, Float)
type Point10d = (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)


instance Storable Point02d where
    sizeOf    _ = #size float [2]
    alignment _ = alignment (undefined::CFloat)
    peek ptr = do
        x0 <- peekByteOff ptr (0)
        x1 <- peekByteOff ptr (1 * #size float)
        return (x0,x1)

    poke ptr (x0,x1) = do
        pokeByteOff ptr (0)               $ x0
        pokeByteOff ptr (1 * #size float) $ x1

instance Storable Point03d where
    sizeOf    _ = #size float [3]
    alignment _ = alignment (undefined::CFloat)
    peek ptr = do
        x0 <- peekByteOff ptr (0)
        x1 <- peekByteOff ptr (1 * #size float)
        x2 <- peekByteOff ptr (2 * #size float)
        return (x0,x1,x2)

    poke ptr (x0,x1,x2) = do
        pokeByteOff ptr (0)               $ x0
        pokeByteOff ptr (1 * #size float) $ x1
        pokeByteOff ptr (2 * #size float) $ x2

instance Storable Point05d where
    sizeOf    _ = #size float [5]
    alignment _ = alignment (undefined::CFloat)
    peek ptr = do
        x0 <- peekByteOff ptr (0)
        x1 <- peekByteOff ptr (1 * #size float)
        x2 <- peekByteOff ptr (2 * #size float)
        x3 <- peekByteOff ptr (3 * #size float)
        x4 <- peekByteOff ptr (4 * #size float)
        return (x0,x1,x2,x3,x4)

    poke ptr (x0,x1,x2,x3,x4) = do
        pokeByteOff ptr (0)               $ x0
        pokeByteOff ptr (1 * #size float) $ x1
        pokeByteOff ptr (2 * #size float) $ x2
        pokeByteOff ptr (3 * #size float) $ x3
        pokeByteOff ptr (4 * #size float) $ x4

instance Storable Point07d where
    sizeOf    _ = #size float [7]
    alignment _ = alignment (undefined::CFloat)
    peek ptr = do
        x0 <- peekByteOff ptr (0)
        x1 <- peekByteOff ptr (1 * #size float)
        x2 <- peekByteOff ptr (2 * #size float)
        x3 <- peekByteOff ptr (3 * #size float)
        x4 <- peekByteOff ptr (4 * #size float)
        x5 <- peekByteOff ptr (5 * #size float)
        x6 <- peekByteOff ptr (6 * #size float)
        return (x0,x1,x2,x3,x4,x5,x6)

    poke ptr (x0,x1,x2,x3,x4,x5,x6) = do
        pokeByteOff ptr (0)               $ x0
        pokeByteOff ptr (1 * #size float) $ x1
        pokeByteOff ptr (2 * #size float) $ x2
        pokeByteOff ptr (3 * #size float) $ x3
        pokeByteOff ptr (4 * #size float) $ x4
        pokeByteOff ptr (5 * #size float) $ x5
        pokeByteOff ptr (6 * #size float) $ x6

instance Storable Point08d where
    sizeOf    _ = #size float [8]
    alignment _ = alignment (undefined::CFloat)
    peek ptr = do
        x0 <- peekByteOff ptr (0)
        x1 <- peekByteOff ptr (1 * #size float)
        x2 <- peekByteOff ptr (2 * #size float)
        x3 <- peekByteOff ptr (3 * #size float)
        x4 <- peekByteOff ptr (4 * #size float)
        x5 <- peekByteOff ptr (5 * #size float)
        x6 <- peekByteOff ptr (6 * #size float)
        x7 <- peekByteOff ptr (7 * #size float)
        return (x0,x1,x2,x3,x4,x5,x6,x7)

    poke ptr (x0,x1,x2,x3,x4,x5,x6,x7) = do
        pokeByteOff ptr (0)               $ x0
        pokeByteOff ptr (1 * #size float) $ x1
        pokeByteOff ptr (2 * #size float) $ x2
        pokeByteOff ptr (3 * #size float) $ x3
        pokeByteOff ptr (4 * #size float) $ x4
        pokeByteOff ptr (5 * #size float) $ x5
        pokeByteOff ptr (6 * #size float) $ x6
        pokeByteOff ptr (7 * #size float) $ x7

instance Storable Point09d where
    sizeOf    _ = #size float [9]
    alignment _ = alignment (undefined::CFloat)
    peek ptr = do
        x0 <- peekByteOff ptr (0)
        x1 <- peekByteOff ptr (1 * #size float)
        x2 <- peekByteOff ptr (2 * #size float)
        x3 <- peekByteOff ptr (3 * #size float)
        x4 <- peekByteOff ptr (4 * #size float)
        x5 <- peekByteOff ptr (5 * #size float)
        x6 <- peekByteOff ptr (6 * #size float)
        x7 <- peekByteOff ptr (7 * #size float)
        x8 <- peekByteOff ptr (8 * #size float)
        return (x0,x1,x2,x3,x4,x5,x6,x7,x8)

    poke ptr (x0,x1,x2,x3,x4,x5,x6,x7,x8) = do
        pokeByteOff ptr (0)               $ x0
        pokeByteOff ptr (1 * #size float) $ x1
        pokeByteOff ptr (2 * #size float) $ x2
        pokeByteOff ptr (3 * #size float) $ x3
        pokeByteOff ptr (4 * #size float) $ x4
        pokeByteOff ptr (5 * #size float) $ x5
        pokeByteOff ptr (6 * #size float) $ x6
        pokeByteOff ptr (7 * #size float) $ x7
        pokeByteOff ptr (8 * #size float) $ x8

instance Storable Point10d where
    sizeOf    _ = #size float [10]
    alignment _ = alignment (undefined::CFloat)
    peek ptr = do
        x0 <- peekByteOff ptr (0)
        x1 <- peekByteOff ptr (1 * #size float)
        x2 <- peekByteOff ptr (2 * #size float)
        x3 <- peekByteOff ptr (3 * #size float)
        x4 <- peekByteOff ptr (4 * #size float)
        x5 <- peekByteOff ptr (5 * #size float)
        x6 <- peekByteOff ptr (6 * #size float)
        x7 <- peekByteOff ptr (7 * #size float)
        x8 <- peekByteOff ptr (8 * #size float)
        x9 <- peekByteOff ptr (9 * #size float)
        return (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9)

    poke ptr (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9) = do
        pokeByteOff ptr (0)               $ x0
        pokeByteOff ptr (1 * #size float) $ x1
        pokeByteOff ptr (2 * #size float) $ x2
        pokeByteOff ptr (3 * #size float) $ x3
        pokeByteOff ptr (4 * #size float) $ x4
        pokeByteOff ptr (5 * #size float) $ x5
        pokeByteOff ptr (6 * #size float) $ x6
        pokeByteOff ptr (7 * #size float) $ x7
        pokeByteOff ptr (8 * #size float) $ x8
        pokeByteOff ptr (9 * #size float) $ x9
