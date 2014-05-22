{-# OPTIONS -fno-warn-missing-signatures #-}
{- |
Module      :  Data.Loader
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable


-}

module Data.Loader
  ( getContext
  , runParser
  )
    where

import Control.Monad.Error ( runErrorT )
import Data.Loader.Parser  ( parseConfigFile )

-- |
getContext fn mParser = do

   rv <- runErrorT $ parseConfigFile fn mParser

   case rv of
       Left  e -> error  $ show e
       Right c -> return c

-- |
runParser fn = getContext fn