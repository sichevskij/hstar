{-# LANGUAGE TypeOperators #-}
{- |
Module      :  Synth.Photometry
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

Модель фотометрии

-}

module Synth.Photometry
 ( ModelPhotometry(..)
 )
   where

import Control.Applicative  ( liftA2, (<$>), (<*>) )
import Control.Monad.Reader ( ReaderT )
import Data.HList           ( (:::)(..), Null(..) )
import Synth.Common         ( Spectrum )
import Synth.Context        ( Context )
 
-- Класс моделируемая фотометрия
class (Show a) => ModelPhotometry a where
  mag :: ReaderT Context IO (Spectrum -> a)

instance (ModelPhotometry x, ModelPhotometry xs) => ModelPhotometry (x ::: xs) where
  mag = liftA2 (:::) <$> mag <*> mag

instance ModelPhotometry Null where
  mag = return $ const Null