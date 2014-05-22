{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE TypeOperators, RankNTypes, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

{- |
Module      :  Synth.Context
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Synth.Context
  ( configParser
  , contextParser
  , Context(..)
  )
    where

import Control.Monad.Error      ( MonadError )
import Control.Monad.Reader     ( MonadReader, MonadIO, liftIO )
import Data.ConfigFile          ( ConfigParser(..) )
import Data.MagnitudeSystem     ( VEGAmag, ABMag )
import Data.PhotoModel          ( PhotoModel, UGRIZ, JHK, FUVNUV, Lira )
import Data.Loader.Parser.Error ( SCPError(..) )
import Synth.Parser             ( Model, Bands(..), optParser, galexParser, sdssParser, twomassParser, liraParser, modelParser )


data Context  = Context
  { galex   :: PhotoModel ABMag   FUVNUV
  , sdss    :: PhotoModel ABMag   UGRIZ
  , twomass :: PhotoModel VEGAmag JHK
  , lira    :: PhotoModel VEGAmag Lira
  }

-- | Парсер для анализа файла конфигурации.
configParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m ( (Bands, FilePath), [Model] ,Context )
configParser = do

  liftIO $ putStrLn  "\nОбработка файла с настройками ...\n"

  opt <- optParser
  mdl <- modelParser
  cnt <- contextParser

  return (opt,mdl,cnt)

contextParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m Context
contextParser = do

  liftIO $ putStrLn  "\nОбработка секций с описание фотометрических моделей обзоров ...\n"

  phLIRA  <- liraParser
  phGALEX <- galexParser
  phSDSS  <- sdssParser
  ph2MASS <- twomassParser

  return $ Context
    { lira = phLIRA
    , galex = phGALEX
    , sdss = phSDSS
    , twomass = ph2MASS
    }