{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts, TypeOperators, OverloadedStrings #-}

{- |
Module      :  Server.Context
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Synth.Parser
  ( optParser
  , modelParser
  , galexParser
  , sdssParser
  , twomassParser
  , iphasParser
  , liraParser
  , parseResponse
  , parseFluxATLAS9
  , Bands(..)
  , Model
  )
    where

import Control.Applicative      ( (<$>) )
import Control.Monad            ( forM )
import Control.Monad.Error      ( MonadError )
import Control.Monad.Reader     ( MonadReader, MonadIO, liftIO )
import Data.ConfigFile          ( ConfigParser(..) )
import Data.Loader.Parser       ( get )
import Data.Loader.Parser.Error ( SCPError(..) )
import Data.Maybe               ( fromJust )
import Data.MagnitudeSystem     ( VEGAmag(..), ABMag(..) )
import Data.PhotoModel          ( PhotoModel(..), Band(..), UGRIZ(..), JHK(..), FUVNUV(..), RIHα(..), Lira(..) )
import Data.ZeroPoint           ( AsinhZeroPoint(..), PogsonZeroPoint(..) )
import Data.HList               ( (:::)(..), Null(..) )
import System.FilePath          ( (</>) )
import Synth.Common             ( Spectrum, Teff(..), Logg(..), FeH(..) )

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Lex.Lazy.Double as LD

data Bands = Bolometric | LIRA | IPHAS | GALEX | SDSS | TWOMASS | GALEXxSDSS | SDSSxTWOMASS | GALEXxSDSSxTWOMASS deriving (Read,Show)

type Model  = ( (Teff ::: Logg ::: FeH ::: Null), Spectrum )

-- Парсер для анализа секции
optParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m (Bands, FilePath)
optParser = do

  s <- get "OPTIONS" "bands"
  f <- get "OPTIONS" "outFile"

  return (s, f)

-- Парсер для анализа секции
modelParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m [Model]
modelParser = do

  ph <- get "ATLAS9" "path"
  fn <- get "ATLAS9" "file"

  parseFile ph fn

  where
    parseFile ph fn = do
      -- Считываем содержимое файла.
      cs <- liftIO $ readFile (ph </> fn)

      -- Затем разбиваем его на строки, пропустив комментарии.
      let lls = (filter ((/='#') . head) . lines) cs

      -- Строки разбиваем на слова.
      let lws = filter (not.null) $ words <$> lls

      forM lws $ \ (wa:wb:wc:wd:_) -> do
  
         -- Определяем Teff, logg, [M/H] и имя файла с моделью спектра.
         let teff = read wa
             logg = read wb
             mh   = read wc
             file = ph </> wd

         -- Чтение файла модели спектра.
         flux <- parseFluxATLAS9 file

         return ( (Teff teff ::: Logg logg ::: FeH mh ::: Null), flux)



-- Парсер для анализа секции
twomassParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m (PhotoModel VEGAmag JHK)
twomassParser = do

  liftIO $ putStrLn "\nЧтение секции, описывающую обзор TWOMASS...\n"

  let sectionName = "PHOTOMODEL_TWOMASS"

  zp_flux_j <- get sectionName "J_ZP_FLUX"
  zp_flux_h <- get sectionName "H_ZP_FLUX"
  zp_flux_k <- get sectionName "K_ZP_FLUX"


  zp_mag_j  <- get sectionName "J_ZP_MAG"
  zp_mag_h  <- get sectionName "H_ZP_MAG"
  zp_mag_k  <- get sectionName "K_ZP_MAG"

  filter_j  <- get sectionName "J_2MASS" >>= parseResponse
  filter_h  <- get sectionName "H_2MASS" >>= parseResponse
  filter_k  <- get sectionName "K_2MASS" >>= parseResponse

  return $ PhotoModel
    { magSystem = VEGAmag
    , bands = JHK
        { j_band = Band filter_j (PogsonZeroPoint zp_flux_j zp_mag_j)
        , h_band = Band filter_h (PogsonZeroPoint zp_flux_h zp_mag_h)
        , k_band = Band filter_k (PogsonZeroPoint zp_flux_k zp_mag_k)
        }
    }

-- Парсер для анализа секции
iphasParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m (PhotoModel VEGAmag RIHα)
iphasParser = do

  liftIO $ putStrLn "\nЧтение секции, описывающую обзор IPHAS...\n"

  let sectionName = "PHOTOMODEL_IPHAS"

  zp_flux_r  <- get sectionName "R_ZP_FLUX"
  zp_flux_i  <- get sectionName "I_ZP_FLUX"
  zp_flux_hα <- get sectionName "Hα_ZP_FLUX"


  zp_mag_r  <- get sectionName "R_ZP_MAG"
  zp_mag_i  <- get sectionName "I_ZP_MAG"
  zp_mag_hα <- get sectionName "Hα_ZP_MAG"

  filter_r  <- get sectionName "R_IPHAS"  >>= parseResponse
  filter_i  <- get sectionName "I_IPHAS"  >>= parseResponse
  filter_hα <- get sectionName "Hα_IPHAS" >>= parseResponse

  return $ PhotoModel
    { magSystem = VEGAmag
    , bands = RIHα
        { r'_band = Band filter_r  (PogsonZeroPoint zp_flux_r  zp_mag_r)
        , i'_band = Band filter_i  (PogsonZeroPoint zp_flux_i  zp_mag_i)
        , hα_band = Band filter_hα (PogsonZeroPoint zp_flux_hα zp_mag_hα)
        }
    }

-- Парсер для анализа секции
sdssParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m (PhotoModel ABMag UGRIZ)
sdssParser = do

  liftIO $ putStrLn "\nЧтение секции, описывающую обзор SDSS...\n"

  let sectionName = "PHOTOMODEL_SDSS"

  soft_pr_u <- get sectionName "U_SOFT_PARAM"
  soft_pr_g <- get sectionName "G_SOFT_PARAM"
  soft_pr_r <- get sectionName "R_SOFT_PARAM"
  soft_pr_i <- get sectionName "I_SOFT_PARAM"
  soft_pr_z <- get sectionName "Z_SOFT_PARAM"

  zp_flux_u <- get sectionName "U_ZP_FLUX"
  zp_flux_g <- get sectionName "G_ZP_FLUX"
  zp_flux_r <- get sectionName "R_ZP_FLUX"
  zp_flux_i <- get sectionName "I_ZP_FLUX"
  zp_flux_z <- get sectionName "Z_ZP_FLUX"


  zp_mag_u  <- get sectionName "U_ZP_MAG"
  zp_mag_g  <- get sectionName "G_ZP_MAG"
  zp_mag_r  <- get sectionName "R_ZP_MAG"
  zp_mag_i  <- get sectionName "I_ZP_MAG"
  zp_mag_z  <- get sectionName "Z_ZP_MAG"

  filter_u  <- get sectionName "U_SDSS" >>= parseResponse
  filter_g  <- get sectionName "G_SDSS" >>= parseResponse
  filter_r  <- get sectionName "R_SDSS" >>= parseResponse
  filter_i  <- get sectionName "I_SDSS" >>= parseResponse
  filter_z  <- get sectionName "Z_SDSS" >>= parseResponse

  return $ PhotoModel
    { magSystem = ABMag
    , bands = UGRIZ
        { u_band = Band filter_u (AsinhZeroPoint zp_flux_u zp_mag_u soft_pr_u)
        , g_band = Band filter_g (AsinhZeroPoint zp_flux_g zp_mag_g soft_pr_g)
        , r_band = Band filter_r (AsinhZeroPoint zp_flux_r zp_mag_r soft_pr_r)
        , i_band = Band filter_i (AsinhZeroPoint zp_flux_i zp_mag_i soft_pr_i)
        , z_band = Band filter_z (AsinhZeroPoint zp_flux_z zp_mag_z soft_pr_z)
        }
    }


-- Парсер для анализа секции
galexParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m (PhotoModel ABMag FUVNUV)
galexParser = do

  liftIO $ putStrLn "\nЧтение секции, описывающую обзор GALEX...\n"

  let sectionName = "PHOTOMODEL_GALEX"

  zp_flux_fuv <- get sectionName "FUV_ZP_FLUX"
  zp_flux_nuv <- get sectionName "NUV_ZP_FLUX"

  zp_mag_fuv  <- get sectionName "FUV_ZP_MAG"
  zp_mag_nuv  <- get sectionName "NUV_ZP_MAG"

  filter_fuv  <- get sectionName "FUV_GALEX" >>= parseResponse
  filter_nuv  <- get sectionName "NUV_GALEX" >>= parseResponse

  return $ PhotoModel
    { magSystem = ABMag
    , bands = FUVNUV
        { fuv_band = Band filter_fuv (PogsonZeroPoint zp_flux_fuv zp_mag_fuv)
        , nuv_band = Band filter_nuv (PogsonZeroPoint zp_flux_nuv zp_mag_nuv)
        }
    }

-- Парсер для анализа секции
liraParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m (PhotoModel VEGAmag Lira)
liraParser = do

  liftIO $ putStrLn "\nЧтение секции, описывающую обзор 'Лира'...\n"

  let sectionName = "PHOTOMODEL_LIRA"

  zp_flux_q195   <- get sectionName "q195_ZP_FLUX"
  zp_flux_q218   <- get sectionName "q218_ZP_FLUX"
  zp_flux_q270   <- get sectionName "q270_ZP_FLUX"
  zp_flux_q350   <- get sectionName "q350_ZP_FLUX"
  zp_flux_q374c  <- get sectionName "q374c_ZP_FLUX"
  zp_flux_q440   <- get sectionName "q440_ZP_FLUX"
  zp_flux_q550   <- get sectionName "q550_ZP_FLUX"
  zp_flux_q700   <- get sectionName "q700_ZP_FLUX"
  zp_flux_q785c  <- get sectionName "q785c_ZP_FLUX"
  zp_flux_q825   <- get sectionName "q825_ZP_FLUX"
  zp_flux_q930   <- get sectionName "q930_ZP_FLUX"
  zp_flux_q1000  <- get sectionName "q1000_ZP_FLUX"
  zp_flux_q_pn14 <- get sectionName "q_pn14_ZP_FLUX"
  zp_flux_q_pn30 <- get sectionName "q_pn30_ZP_FLUX"

  zp_mag_q195   <- get sectionName "q195_ZP_MAG"
  zp_mag_q218   <- get sectionName "q218_ZP_MAG"
  zp_mag_q270   <- get sectionName "q270_ZP_MAG"
  zp_mag_q350   <- get sectionName "q350_ZP_MAG"
  zp_mag_q374c  <- get sectionName "q374c_ZP_MAG"
  zp_mag_q440   <- get sectionName "q440_ZP_MAG"
  zp_mag_q550   <- get sectionName "q550_ZP_MAG"
  zp_mag_q700   <- get sectionName "q700_ZP_MAG"
  zp_mag_q785c  <- get sectionName "q785c_ZP_MAG"
  zp_mag_q825   <- get sectionName "q825_ZP_MAG"
  zp_mag_q930   <- get sectionName "q930_ZP_MAG"
  zp_mag_q1000  <- get sectionName "q1000_ZP_MAG"
  zp_mag_q_pn14 <- get sectionName "q_pn14_ZP_MAG"
  zp_mag_q_pn30 <- get sectionName "q_pn30_ZP_MAG"

  filter_q195   <- get sectionName "q195_LIRA"   >>= parseResponse
  filter_q218   <- get sectionName "q218_LIRA"   >>= parseResponse
  filter_q270   <- get sectionName "q270_LIRA"   >>= parseResponse
  filter_q350   <- get sectionName "q350_LIRA"   >>= parseResponse
  filter_q374c  <- get sectionName "q374c_LIRA"  >>= parseResponse
  filter_q440   <- get sectionName "q440_LIRA"   >>= parseResponse
  filter_q550   <- get sectionName "q550_LIRA"   >>= parseResponse
  filter_q700   <- get sectionName "q700_LIRA"   >>= parseResponse
  filter_q785c  <- get sectionName "q785c_LIRA"  >>= parseResponse 
  filter_q825   <- get sectionName "q825_LIRA"   >>= parseResponse
  filter_q930   <- get sectionName "q930_LIRA"   >>= parseResponse
  filter_q1000  <- get sectionName "q1000_LIRA"  >>= parseResponse
  filter_q_pn14 <- get sectionName "q_pn14_LIRA" >>= parseResponse
  filter_q_pn30 <- get sectionName "q_pn30_LIRA" >>= parseResponse

  return $ PhotoModel
    { magSystem = VEGAmag
    , bands = Lira
        { q195_band   = Band filter_q195   (PogsonZeroPoint zp_flux_q195   zp_mag_q195)
        , q218_band   = Band filter_q218   (PogsonZeroPoint zp_flux_q218   zp_mag_q218)
        , q270_band   = Band filter_q270   (PogsonZeroPoint zp_flux_q270   zp_mag_q270)
        , q350_band   = Band filter_q350   (PogsonZeroPoint zp_flux_q350   zp_mag_q350)
        , q374c_band  = Band filter_q374c  (PogsonZeroPoint zp_flux_q374c  zp_mag_q374c)
        , q440_band   = Band filter_q440   (PogsonZeroPoint zp_flux_q440   zp_mag_q440)
        , q550_band   = Band filter_q550   (PogsonZeroPoint zp_flux_q550   zp_mag_q550)
        , q700_band   = Band filter_q700   (PogsonZeroPoint zp_flux_q700   zp_mag_q700)
        , q785c_band  = Band filter_q785c  (PogsonZeroPoint zp_flux_q785c  zp_mag_q785c)
        , q825_band   = Band filter_q825   (PogsonZeroPoint zp_flux_q825   zp_mag_q825)
        , q930_band   = Band filter_q930   (PogsonZeroPoint zp_flux_q930   zp_mag_q930)
        , q1000_band  = Band filter_q1000  (PogsonZeroPoint zp_flux_q1000  zp_mag_q1000)
        , q_pn14_band = Band filter_q_pn14 (PogsonZeroPoint zp_flux_q_pn14 zp_mag_q_pn14)
        , q_pn30_band = Band filter_q_pn30 (PogsonZeroPoint zp_flux_q_pn30 zp_mag_q_pn30)
        }
    }

-- |
parseResponse :: MonadIO m => FilePath -> m [(Double, Double)]
parseResponse fn = liftIO $ do
      -- Чтобы ускорить последующие вычисления - а именно, перевод
      -- энергетического потока в поток фотонов - выполняется умножение на
      -- длину волны.
      return . ((\ [w,r] -> (w, w * r)) <$>) =<< readTable fn

-- |
parseFluxATLAS9 :: MonadIO m => FilePath -> m [(Double, Double)]
parseFluxATLAS9 fn = liftIO $ do
      -- Считываем содержимое файла.
      cs <- LB.readFile fn

      -- Затем разбиваем его на строки, пропустив первые две строки.
      let lls = (drop 2 . LB.lines) cs

      -- Строки разбиваем на слова, удалив первые 9 символов из каждой
      -- строки и оставив только три первых слова.
      let lws = filter (not.null) $ (take 3 . LB.words . LB.drop 9) <$> lls

      -- Из слов считываем числа. В конце переводим нанометры в ангстремы, а
      -- эддингтоновский поток (Hnu в эрг/(с·Гц·см²) ) в астрофизический
      -- поток ( Flam в эрг/(с·А·см²) ).
      return $ ( \[w, _, h_nu] -> (10 * w, 4 * h_nu * c / (w * w)) ) <$> (fst . fromJust . LD.readDouble <$>) <$> lws

      where
        -- скорость света, А/с
        c = 2.99792458E+18

-- Считываем содержимое файла. Затем разбиваем его на строки, пропустив
-- пустые строк и строки, начинающиеся с '#', строки разбиваем на слова,
-- оставив только два первых слова.  A из слов считываем числа.
readTable :: FilePath -> IO [[Double]]
readTable fn = return . ((readDouble <$>) . (take 2 . LB.words) <$>) . (filter ((/='#') . LB.head) . LB.lines) =<< LB.readFile fn
  where
    --
    readDouble = fst . fromJust . LD.readDouble

{--
-- Парсер для анализа секции
sedParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m [Model]
sedParser = do

  mh <- get "SPECTRAL_ENERGY_DISTRIBUTION" "mh"

  get "SPECTRAL_ENERGY_DISTRIBUTION" "file" >>= parseSet mh

  where
    parseSet mh fn = do

       liftIO $ putStrLn $ "parse " ++ fn ++ "..."

       (_,xs) <- parseATLAS9Logg fn

       ms <- forM xs $ \(x,fn) -> do

          (_,ys) <- parseATLAS9Teff fn

          forM ys $ \(y,fn) -> do

             wf <- parseATLAS9Flux fn

             return ( (Teff y ::: Logg x ::: FeH mh ::: Null), wf)

       return $ concat ms

-- |
parseATLAS9Logg :: (MonadIO m, MonadError SCPError m) => FilePath -> m ((Double,Double,Double), [(Double, [Char])])
parseATLAS9Logg = parseTableFile cParser pParser
   where
      cParser = lift $ return $ do
         x  <- double
         y  <- string

         return (x,y)

      pParser (cp, t) = do
         g <- T.get cp "DEFAULT" "Grid"
         p <- T.get cp "DEFAULT" "Path"

         let f (x,y) = ( x, p++"/"++y )

         return (g, map f t)

-- |
parseATLAS9Teff :: (MonadIO m, MonadError SCPError m) => FilePath -> m ((Double,Double,Double), [(Double, [Char])])
parseATLAS9Teff = parseTableFile cParser pParser
   where
      cParser = lift $ return $ do
         x  <- double
         y  <- string

         return (x,y)

      pParser (cp, t) = do
         g <- T.get cp "DEFAULT" "Grid"
         p <- T.get cp "DEFAULT" "Path"

         let f (x,y) = ( x, p++"/"++y )

         return (g, map f t)

-- |
parseATLAS9Flux :: MonadIO m => FilePath -> m [(Double, Double)]
parseATLAS9Flux fn = liftIO $ do
      -- Считываем содержимое файла. Затем разбиваем его на строки,
      -- пропустив строки, начинающиеся с '#', строки разбиваем на слова,
      -- оставив только два первых слова.  A из слов считываем числа.  В
      -- конце переводим эддингтоновский поток (Hnu) в астрофизический поток
      -- (Flam).

      return . ((\ [w, h_nu] -> (w, 4 * h_nu * c / (w * w))) <$>) =<< readTable fn

--      return . (( \[w, h_nu] -> (w, 4 * h_nu * c / (w * w))) <$>) . ((readDouble <$>) . (take 2 . LB.words) <$>) . (filter ((/='#') . LB.head) . LB.lines) =<< LB.readFile fn
 
      where
        -- скорость света, А/сек
        c = 299792458E10
--}