{-# OPTIONS -fno-warn-missing-signatures -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
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
import Control.Monad        ( forM, forM_ )
import Control.Monad.Reader ( MonadIO, ReaderT, runReaderT, liftIO, lift, asks )
import Data.Loader          ( getContext )
import Data.Loader.Parser   ( get, parseTableFile )
import Data.MagnitudeSystem ( refSpectrum )
import Data.PhotoModel      ( PhotoModel(..), Band(..), UGRIZ(..), JHK(..) )
import Data.HList           ( (:::), Null, ToDouble(..), hToList, hMap )
import System.Environment   ( getArgs, getProgName )
import System.Exit          ( exitWith, ExitCode(..) )
import Synth.Context        ( Context(..), contextParser )
import Synth.Common         ( Spectrum, quad, (<*>) )
import Synth.Photometry     ( mag )
import Synth.Survey
import Synth.Survey.GALEX   ()
import Synth.Survey.SDSS    ()
import Synth.Survey.TWOMASS ()
import Text.Printf          ( printf )
import Text.Scanf           ( double, string, skip )

type Error   = Double
type Factory = ReaderT Context IO

data SURVEY = SURVEY_GALEX | SURVEY_SDSS_CALSPEC | SURVEY_SDSS_SEGUE | SURVEY_TWOMASS | SURVEY_TWOMASS_CALSPEC deriving (Read, Show)

data Object = ObjGALEX   ( (FUV GALEX, Error), (NUV GALEX, Error) )
            | ObjSDSS    ( (U SDSS, Error), (G SDSS, Error), (R SDSS, Error), (I SDSS, Error), (Z SDSS, Error) )
            | ObjTWOMASS ( (J TWOMASS, Error), (H TWOMASS, Error), (K TWOMASS, Error) )

main :: IO ()
main = do

 fn <- getArgs >>= parse

 (os,s,c) <- getContext fn mainParser

 runReaderT (process os s) c

parse :: [String] -> IO FilePath
parse argv = case argv of

  [fn] -> return fn
  _    -> usage

 where
    usage = do
      pn <- getProgName
      putStrLn ("Usage : "++ pn ++" CONFIGURATION_FILE") >> exitWith ExitSuccess

process os surv = do

  printZeroFlux surv

  forM_ os $ \ (nm, o, s) -> do

    f <- case o of
           ObjGALEX   _ -> return . toList =<< (mag :: Factory ( Spectrum -> FUV GALEX ::: NUV GALEX ::: Null))
           ObjSDSS    _ -> return . toList =<< (mag :: Factory ( Spectrum -> U SDSS ::: G SDSS ::: R SDSS ::: I SDSS ::: Z SDSS ::: Null))
           ObjTWOMASS _ -> return . toList =<< (mag :: Factory ( Spectrum -> J TWOMASS ::: H TWOMASS ::: K TWOMASS ::: Null))

    liftIO $ printTableRow nm o (f s)

  where
    toList m = (hToList.hMap ToDouble) <$> m

printZeroFlux :: SURVEY -> Factory ()

printZeroFlux SURVEY_SDSS_CALSPEC = do

  -- В идеальной AB системе нулевая звездная величина приписывается
  -- объекту, от которого регистрируется столько же отсчетов в секунду, как
  -- и от объекта с постоянным распределением по частоте энергии в спектре
  -- равным 3631 Ян ( 3.631×10^-20 эрг/(с·Гц·см²) ).

  -- Запрашиваем cпектр объекта нулевой звездной величины в эрг/(с·A·см²).
  s <- asks $ refSpectrum . magSystem . sdss

  -- Запрашиваем кривые реакции для полос обзора SDSS. Нужно отметить, что
  -- для оптимизации эти кривые уже умножены на длину волны.
  u <- asks $ pfilter . u_band . bands . sdss
  g <- asks $ pfilter . g_band . bands . sdss
  r <- asks $ pfilter . r_band . bands . sdss
  i <- asks $ pfilter . i_band . bands . sdss
  z <- asks $ pfilter . z_band . bands . sdss

  -- Произведение Постоянная планка в эрг·c на скорость света в A/с.
  let hc = 6.62606957E-27 * 2.99792458E+18

  -- Вычисление потока в фотонах от объекта в нулевой звездной величины.
  let f0u = (quad $ u <*> s) / hc
      f0g = (quad $ g <*> s) / hc
      f0r = (quad $ r <*> s) / hc
      f0i = (quad $ i <*> s) / hc
      f0z = (quad $ z <*> s) / hc

  liftIO $ do
    printf "The flux of an object with conventional magnitude of zero:\n"
    mapM_ (\ (b,f) -> printf "f0%c = %6.0f\n" b f) (zip "ugriz" [f0u,f0g,f0r,f0i,f0z])

printZeroFlux SURVEY_SDSS_SEGUE = printZeroFlux SURVEY_SDSS_CALSPEC

printZeroFlux SURVEY_TWOMASS = do

  -- Запрашиваем cпектр объекта нулевой звездной величины в эрг/(с·A·см²).
  s <- asks $ refSpectrum . magSystem . twomass

  -- Запрашиваем кривые реакции для полос обзора SDSS. Нужно отметить, что
  -- для оптимизации эти кривые уже умножены на длину волны.
  j <- asks $ pfilter . j_band . bands . twomass
  h <- asks $ pfilter . h_band . bands . twomass
  k <- asks $ pfilter . k_band . bands . twomass

  -- Произведение Постоянная планка в эрг·c на скорость света в A/с.
  let hc = 6.62606957E-27 * 2.99792458E+18

  -- Вычисление потока в фотонах от объекта в нулевой звездной величины.
  let f0j = (quad $ j <*> s) / hc
      f0h = (quad $ h <*> s) / hc
      f0k = (quad $ k <*> s) / hc

  liftIO $ do
    printf "The flux of an object with conventional magnitude of zero:\n"
    mapM_ (\ (b,f) -> printf "f0%c = %6.0f\n" b f) (zip "JHK" [f0j,f0h,f0k])


printZeroFlux _ = return ()  

printTableRow :: String -> Object -> [Double] -> IO ()
printTableRow nm (ObjGALEX o) [fuv',nuv'] = do

  printf "%s\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\n" nm fuv efuv nuv enuv fuv' nuv'

   where
     ( ( (FUV GALEX (Just fuv), efuv)
       , (NUV GALEX (Just nuv), enuv) ) ) = o

printTableRow nm (ObjSDSS o) [u',g',r',i',z'] = do

  printf "%s\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\n" nm u eu g eg r er i ei z ez u' g' r' i' z'

  where
     ( ( (U SDSS (Just u), eu)
       , (G SDSS (Just g), eg)
       , (R SDSS (Just r), er)
       , (I SDSS (Just i), ei)
       , (Z SDSS (Just z), ez) ) ) = o

printTableRow nm (ObjTWOMASS o) [j',h',k'] = do

  printf "%20s\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\n" nm j ej h eh k ek j' h' k'

  where
     ( ( (J TWOMASS (Just j), ej)
       , (H TWOMASS (Just h), eh)
       , (K TWOMASS (Just k), ek) ) ) = o



-- |Главный парсер для анализа файла конфигурации.
mainParser = do

  s  <- get "OPTIONS" "survey"

  aos <- objParser s
  cnt <- contextParser

  return (aos,s,cnt)

-- |
objParser surv = do

  fe <- get (show surv) "fileExt"

  fp <- get (show surv) "filePath"

  rs <- get (show surv) "fileObjs" >>= parseInputFile surv

  os <- forM rs $ \(nm,o) -> do

    s <- parseSpecFileTxt surv $ fp ++ nm ++ "." ++ fe

    return (nm,o,s)

  return os

  where
    parseSpecFileTxt SURVEY_SDSS_SEGUE inFile = do
      liftIO $ do

        -- Считываем содержимое файла. Затем разбиваем его на строки,
        -- пропустив первые 8 строк, строки разбиваем на слова, оставив
        -- только два первых слова.  A из слов считываем числа.

        ws <- return . ((read <$>) . (take 2 . words) <$>) . (drop 8 . lines) =<< readFile inFile

        -- Учитывая формат записи длины волны и потока в файле, переводим
        -- прочитанные данные в ангстремы и поток.
        return $ ( \ [x0,x1] -> (10**x1,  x0 * 1E-17) ) <$> ws

    parseSpecFileTxt SURVEY_SDSS_CALSPEC inFile = do
      liftIO $ do

        -- Считываем содержимое файла. Затем разбиваем его на строки,
        -- пропустив первые 8 строк, строки разбиваем на слова, оставив
        -- только два первых слова.  A из слов считываем числа.

        ws <- return . ((read <$>) . (take 2 . words) <$>) . (filter ((/='#') . head) . lines) =<< readFile inFile

        -- Учитывая формат записи длины волны и потока в файле, переводим
        -- прочитанные данные в ангстремы и поток.
        return $ ( \ [x0,x1] -> (x0,  x1) ) <$> ws

    parseSpecFileTxt SURVEY_TWOMASS inFile = do

      liftIO $ do

        -- Считываем содержимое файла. Затем разбиваем его на строки,
        -- пропустив строки, начинающиеся на '#', строки разбиваем на слова,
        -- оставив только два первых слова.  A из слов считываем числа.

        ws <- return . ((read . ('0':) <$>) . (take 2 . words) <$>) . (filter ((/='#') . head) . lines) =<< readFile inFile

        -- Учитывая формат записи длины волны и потока в файле, переводим
        -- прочитанные данные в ангстремы и поток.
        return $ ( \ [x0,x1] -> (x0 * 1E4, x1 * 1E3) ) <$> ws

    parseSpecFileTxt SURVEY_TWOMASS_CALSPEC f = parseSpecFileTxt SURVEY_SDSS_CALSPEC f

    parseInputFile SURVEY_GALEX = parseTableFile contentParser postParser
      where
       contentParser = lift $ return $ do
         nm <- string

         fuv  <- double; efuv <- double
         nuv  <- double; enuv <- double

         return (nm,  ObjGALEX ( (FUV GALEX (Just fuv), efuv)
                               , (NUV GALEX (Just nuv), enuv)
                               )
                )

       postParser (_, t) = return t

    parseInputFile SURVEY_SDSS_CALSPEC = parseTableFile contentParser postParser
      where
       contentParser = lift $ return $ do
         nm <- string

         u  <- double; eu <- double
         g  <- double; eg <- double
         r  <- double; er <- double
         i  <- double; ei <- double
         z  <- double; ez <- double

         return (nm, ObjSDSS ( (U SDSS (Just u), eu)
                             , (G SDSS (Just g), eg)
                             , (R SDSS (Just r), er)
                             , (I SDSS (Just i), ei)
                             , (Z SDSS (Just z), ez)
                             )
                )

       postParser (_, t) = return t

    parseInputFile SURVEY_SDSS_SEGUE = parseInputFile SURVEY_SDSS_CALSPEC

    parseInputFile SURVEY_TWOMASS = parseTableFile contentParser postParser
      where
       contentParser = lift $ return $ do
         nm <- string; skip; skip;

         j <- double; ej <- double
         h <- double; eh <- double
         k <- double; ek <- double

         return (nm, ObjTWOMASS ( (J TWOMASS (Just j), ej)
                                , (H TWOMASS (Just h), eh)
                                , (K TWOMASS (Just k), ek)
                                )
                )

       postParser (_, t) = return t

    parseInputFile SURVEY_TWOMASS_CALSPEC = parseInputFile SURVEY_TWOMASS