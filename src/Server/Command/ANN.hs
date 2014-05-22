{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      :  Server.Command.ANN
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

The module contains an implementation of estimation by a library for
approximate nearest neighbor Searching (ANN).  ANN is a library written in
C++, which supports data structures and algorithms for both exact and
approximate nearest neighbor searching in arbitrarily high dimensions.  The
library implements a number of different data structures, based on kd-trees
and box-decomposition trees, and employs a couple of different search
strategies

-}

module Server.Command.ANN
 ( estimate
 , Obj(..)
 )
   where

import Control.Applicative  ( (<$>) )
import Control.Monad.Reader ( ReaderT,  MonadIO, liftIO, asks )
import Data.Object          ( Object(..), GALEX(..), SDSS(..), TWOMASS(..), SDSSxTWOMASS(..), GRIxTWOMASS(..), GRIZxTWOMASS(..), NUVxSDSSxTWOMASS(..) )
import Data.Time.Clock      ( getCurrentTime, utctDayTime )
import Foreign              ( Ptr, newArray, mallocArray, peekArray )
import Math.Proj hiding     ( proj )
import Server.Context       ( Context(..), Tree )
import Text.Printf          ( printf )


import qualified Math.Proj as Proj

{--
foreign import ccall "search_kdtree"
  searchtree :: Ptr Point05d -> Ptr Tree -> Int ->  Ptr Float -> Ptr Point05d -> Ptr Float -> IO ()
--}
foreign import ccall "search_kdtree"
  searchtree :: Ptr Tree -> Int ->  Ptr Float -> Ptr Int -> Ptr Float -> IO ()

--
estimate :: (Obj o, Object o e) => (o, e) -> [o] ->  ReaderT Context IO (Maybe [ (Int, Float) ])
estimate obj [] = do
  -- запрашиваем тип распределения для генерации выборки объектов
  objDist <- asks $ rndObjDist
  -- запрашиваем количество объектов в генерируемой выборке
  numObjs <- asks $ numRndObjs
  -- генерация выборки заданного объема на основе оценки параметров
  -- объекта и их точности
  ros <- liftIO $ genRandomObjects objDist numObjs obj

  estimate obj ros

estimate (o,_) ros = do

  estObj o ros 
  
--
class ( Proj o o, Show o ) => Obj o where

  toList :: o -> [Float]

  kdtree :: o -> Context -> Maybe (Ptr Tree)

  estObj :: o -> [o] -> ReaderT Context IO (Maybe [ (Int, Float) ])
  estObj o os = do

    mtree <- asks $ kdtree o
{--  
     db   <- asks $ database

     liftIO $ do
         search db tree (toList <$> proj <$> os)
--}

    liftIO $ do

      printf "Estimation.ANN: estObj: %s\nError: No kdtree!\n" (show o)

      printf "Estimation.ANN: estObj: %s\n" (show o)

      stime <- liftIO $ getCurrentTime >>= return . utctDayTime

--     r <- search tree (toList <$> proj <$> os) 
      r <- search mtree (toList <$> os) 

      etime <- liftIO $ getCurrentTime >>= return . utctDayTime

      printf "Время поиска : %s\n"  (show $ etime - stime)

      return r

    where
      proj :: Proj o o => o -> o
      proj = Proj.proj

instance Obj GALEX where

  toList (GALEX (fuv,nuv)) = [fuv,nuv]

  kdtree _ = kdtree_galex

instance Obj SDSS where

  toList (SDSS (u,g,r,i,z)) = [u,g,r,i,z]

  kdtree _ = kdtree_sdss

instance Obj TWOMASS where

  toList (TWOMASS (j,h,k)) = [j,h,k]

  kdtree _ = kdtree_twomass

instance Obj SDSSxTWOMASS where

  toList (SDSSxTWOMASS (u,g,r,i,z,j,h,k)) = [u,g,r,i,z,j,h,k]

  kdtree _ = kdtree_sdss_twomass

instance Obj GRIxTWOMASS where

  toList (GRIxTWOMASS (g,r,i,j,h,k)) = [g,r,i,j,h,k]

  kdtree _ = kdtree_gri_twomass

instance Obj GRIZxTWOMASS where

  toList (GRIZxTWOMASS (g,r,i,z,j,h,k)) = [g,r,i,j,z,h,k]

  kdtree _ = kdtree_griz_twomass

instance Obj NUVxSDSSxTWOMASS where

  toList (NUVxSDSSxTWOMASS (nuv, u,g,r,i,z, j,h,k)) = [nuv, u,g,r,i,z, j,h,k]

  kdtree _ = kdtree_nuv_sdss_twomass

{--
search :: Ptr Point05d -> Ptr Tree -> [[Float]] -> IO [(Point05d,Float)]
search db tree queries = do

   let n = length queries

   qsPtr   <- newArray $ concat queries
   psPtr   <- mallocArray n
   distPtr <- mallocArray n

   --
   searchtree db tree n qsPtr psPtr distPtr

   ps   <- peekArray n psPtr
   dist <- peekArray n distPtr

   return $ zip ps dist
--}
search :: Maybe (Ptr Tree) -> [[Float]] -> IO (Maybe [(Int,Float)])
search (Just tree) queries = do

   let n = length queries

   qsPtr   <- newArray $ concat queries
   psPtr   <- mallocArray n
   distPtr <- mallocArray n

   --
   searchtree tree n qsPtr psPtr distPtr

   ps   <- peekArray n psPtr
   dist <- peekArray n distPtr

   return (Just $ zip ps dist)

search Nothing _ = return Nothing