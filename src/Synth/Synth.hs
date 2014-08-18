{-# OPTIONS -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Synth.Synth
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Synth.Synth
 ( process
 )
   where

import Control.Applicative  ( (<$>) )
import Control.Monad        ( forM_ )
import Control.Monad.Reader ( ReaderT, liftIO )
import Data.HList           ( (:::)(..), Null(..), ToDouble(..), hAppend, hToList, hMap )
import System.Random        ( randomRIO )
import Synth.Common         ( Spectrum, Teff(..), Logg(..), FeH(..), Av(..), Rv(..), Theta(..), quad, hc )
import Synth.Context        ( Context )
import Synth.Parser         ( Bands(..) )
import Synth.Photometry     ( mag )
import Synth.Survey hiding  ( Survey, GALEX, SDSS, TWOMASS, IPHAS, LIRA )
import Synth.Survey.LIRA    ()
import Synth.Survey.GALEX   ()
import Synth.Survey.SDSS    ()
import Synth.Survey.TWOMASS ()
import Synth.Survey.IPHAS   ()
import Text.Printf          ( printf )

import qualified Synth.Survey         as S
import qualified Synth.Extinction.CCM as IE

type Factory = ReaderT Context IO

process (surv,fp) ms = do

   -- в соответствии с заданным типом обзора определяем функцию для
   -- вычисления блеска
   mf <- magnitudes surv

   --
   liftIO $ do
     printTableHead fp surv

   forM_ ms $ \ ( p, sed ) -> do

     -- определяем список со значениями полного поглощения (Av) и отношения
     -- полного к селективному (Rv).
{--
     let ars = [ (Av av ::: Rv rv ::: Null) | av <- [0,0.125..2], rv <-[2,2.25..6] ]
--}
--{--
     ars <- liftIO $ do

       av <- randomRIO (0,2) 
       rv <- randomRIO (2,6)

       return [ (Av av ::: Rv rv ::: Null) ]
--}

     forM_ ars $ \ ar -> do

       -- покрасняем спектр в соответствии с заданными значениями (Av, Rv)
       -- закона межзвездного поглощения
       rsed <- sed `redden` ar

       -- определяем угловой размер звезды в стерадианах.
{--
       let ths = (\ m -> 10**(-0.4 * m)) <$> [4.5,4.6 .. 10]
--}
{--
       th <- liftIO $ randomRIO (1e-2,1e-5)  
--}

--       forM_ ths $ \ th -> do      
       forM_ [1] $ \ th -> do      

         -- вычисляем блеск, используя покрасненный спектр
         let cs = mf ( ( \(w, f) -> (w, f*th) ) <$> rsed )

         -- 
         liftIO $ do
           printTableBody fp (p `hAppend` ar `hAppend` (Theta th ::: Null)) cs

--
redden :: Spectrum -> (Av ::: Rv ::: Null) -> Factory (Spectrum)
redden sed ar = do
  
  ie <- IE.model

  return $ (\ (w, f) -> (w, f * (ie ar w)) ) <$> sed

--
magnitudes :: Bands -> Factory (Spectrum -> [Double])
magnitudes surv = do

   let toList m = (hToList.hMap ToDouble) <$> m

   case surv of

     Bolometric -> do
                      return $ \s ->  [ -2.5 * logBase 10 $ quad ((\ (w, f) -> (w, w*f)) <$> s) / hc ]

     LIRA    -> do
                  m <- mag :: Factory ( Spectrum ->  Q195 S.LIRA ::: Q218  S.LIRA ::: Q270  S.LIRA ::: Q350 S.LIRA ::: Q374C S.LIRA ::: Q440  S.LIRA
                                                :::  Q550 S.LIRA ::: Q700  S.LIRA ::: Q785C S.LIRA ::: Q825 S.LIRA ::: Q930  S.LIRA ::: Q1000 S.LIRA
                                                ::: QPN14 S.LIRA ::: QPN30 S.LIRA ::: Null)
                  return $ toList m

     GALEX   -> do
                  m <- mag :: Factory ( Spectrum -> FUV S.GALEX ::: NUV S.GALEX ::: Null)
                  return $ toList m

     SDSS    -> do
                  m <- mag :: Factory ( Spectrum -> U S.SDSS ::: G S.SDSS ::: R S.SDSS ::: I S.SDSS ::: Z S.SDSS ::: Null)
                  return $ toList m
     TWOMASS -> do 
                  m <- mag :: Factory ( Spectrum -> J S.TWOMASS ::: H S.TWOMASS ::: K S.TWOMASS ::: Null)
                  return $ toList m
     IPHAS   -> do 
                  m <- mag :: Factory ( Spectrum -> R S.IPHAS ::: I S.IPHAS ::: Hα S.IPHAS ::: Null)
                  return $ toList m

     GALEXxSDSS   -> do
                  m <- mag :: Factory ( Spectrum ->  FUV S.GALEX ::: NUV S.GALEX ::: U S.SDSS ::: G S.SDSS ::: R S.SDSS ::: I S.SDSS ::: Z S.SDSS ::: Null)
                  return $ toList m

     SDSSxTWOMASS -> do
                  m <- mag :: Factory ( Spectrum -> U S.SDSS ::: G S.SDSS ::: R S.SDSS ::: I S.SDSS ::: Z S.SDSS ::: J S.TWOMASS ::: H S.TWOMASS ::: K S.TWOMASS ::: Null)
                  return $ toList m

     GALEXxSDSSxTWOMASS -> do
                  m <- mag :: Factory ( Spectrum ->  FUV S.GALEX ::: NUV S.GALEX ::: U S.SDSS ::: G S.SDSS ::: R S.SDSS ::: I S.SDSS ::: Z S.SDSS ::: J S.TWOMASS ::: H S.TWOMASS ::: K S.TWOMASS ::: Null)
                  return $ toList m


-- 
printTableBody :: FilePath -> Teff ::: Logg ::: FeH ::: Av ::: Rv ::: Theta ::: Null -> [Double] -> IO () 
printTableBody fp p cs = do

   let (Teff t ::: Logg g ::: FeH feh ::: Av av ::: Rv rv ::: Theta th ::: Null) = p

   let ls = printf "%f\t%.3f\t%.3f\t%.3f\t%.3f\t%.6f\t%s\n" t g feh av rv th (ss :: String)

   printf "%s" ls

   appendFile fp ls

   where
      ss = (printf "%.4f\t") `concatMap` cs

--
printTableHead :: FilePath -> Bands -> IO ()
printTableHead fp surv = do

   let hd = "Teff\tlogg\t[M/H]\tAv\tRv\tTheta\t\t" ++ hb
       hb = case surv of
              GALEX              -> "FUV\tNUV\n"
              SDSS               -> "u\t\tg\t\tr\t\ti\t\tz\n"
              TWOMASS            -> "J\t\tH\t\tK\n"
              IPHAS              -> "R\t\tI\t\tHα\n"
              GALEXxSDSS         -> "FUV\tNUV\tu\t\tg\t\tr\t\ti\t\tz\n"
              SDSSxTWOMASS       -> "u\t\tg\t\tr\t\ti\t\tz\t\tJ\t\tH\t\tK\n"
              GALEXxSDSSxTWOMASS -> "FUV\tNUV\t\tu\t\tg\t\tr\t\ti\t\tz\t\tJ\t\tH\t\tK\n"
              LIRA               -> "Q195\tQ218\tQ270\tQ350\tQ374C\tQ440\tQ550\tQ700\tQ785C\tQ825\tQ930\tQ1000\tQPN14\tQPN30\n"
              Bolometric         -> "Mbol\n"

   printf "%s" hd
   
   writeFile fp hd