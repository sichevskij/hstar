{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Main
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Main where

import Control.Applicative  ( (<$>) )
import Control.Monad        ( forM )
import Control.Monad.Reader ( MonadIO, ReaderT, runReaderT, liftIO, asks )
import Data.Loader          ( getContext )
import Data.MagnitudeSystem ( VEGAmag, refSpectrum )
import Data.PhotoModel      ( PhotoModel(..), Band(..), Lira(..) )
import System.Environment   ( getArgs, getProgName )
import System.Exit          ( exitWith, ExitCode(..) )
import Synth.Common         ( quad, (<*>) )
import Synth.Parser         ( liraParser )
import Text.Printf          ( printf )

type Error   = Double
type Factory = ReaderT (PhotoModel VEGAmag Lira) IO

main :: IO ()
main = do

 fn <- getArgs >>= parse

 c <- getContext fn liraParser

 runReaderT printZeroFlux c

parse :: [String] -> IO FilePath
parse argv = case argv of

  [fn] -> return fn
  _    -> usage

 where
    usage = do
      pn <- getProgName
      putStrLn ("Usage : "++ pn ++" CONFIGURATION_FILE") >> exitWith ExitSuccess

printZeroFlux :: Factory ()
printZeroFlux = do

  let ns = ["q195", "q218", "q270", "q350", "q374c", "q440", "q550", "q700", "q785c", "q825", "q930", "q1000", "q_pn14", "q_pn30"]

  let bs = [   q195_band,  q218_band,  q270_band
           ,   q350_band, q374c_band,  q440_band
           ,   q550_band,  q700_band, q785c_band
           ,   q825_band,  q930_band, q1000_band
           , q_pn14_band
           , q_pn30_band
           ]

  -- Запрашиваем cпектр объекта нулевой звездной величины в эрг/(с·A·см²).
  s <- asks $ refSpectrum . magSystem

  -- Запрашиваем кривые реакции для полос обзора Лира. Нужно отметить, что
  -- для оптимизации эти кривые уже умножены на длину волны.
  fs <- forM bs $ \ band -> do
          asks $ pfilter . band . bands

  -- ВАЖНО!
  -- Здесь и далее считается, что кривая реакции полосы не умножена на длину
  -- волны, как это делается в других места, чтобы ускорить вычисления.

  -- Вычисление потока от объекта в нулевой звездной величины. 
  let f0s = (\f -> quad $ f <*> s ) <$> fs

  liftIO $ do
    printf "The bolometric magnitude of an object with conventional magnitude of zero:\n"
    printf "Mbol = %f\n" $ -2.5 * logBase 10 (quad s)

    printf "The flux of an object with conventional magnitude of zero:\n"
    mapM_ (\ (b,f) -> printf "%s_f0 = %f\n" b f) (zip ns f0s)

    printf "The magnidute of an object with conventional magnitude of zero:\n"
    mapM_ (\ (b,m) -> printf "%s_m0 = %f\n" b m) (zip ns  ((-2.5 * logBase 10)<$>f0s))

  -- Произведение Постоянная планка в эрг·c на скорость света в A/с.
  let hc = 6.62606957E-27 * 2.99792458E+18

  -- Вычисление потока фотонов от объекта в нулевой звездной величины.
  let f0s = (\f -> (quad $ f <*> s)/ hc) <$> ( (\(w,r) -> (w, w*r)) <$>) <$> fs

  liftIO $ do
    printf "The photon flux of an object with conventional magnitude of zero:\n"
    mapM_ (\ (b,f) -> printf "%s_f0 = %6.0f\n" b f) (zip ns f0s)

    printf "The magnidute of an object with conventional magnitude of zero:\n"
    mapM_ (\ (b,m) -> printf "%s_m0 = %f\n" b m) (zip ns  ((-2.5 * logBase 10)<$>f0s))
