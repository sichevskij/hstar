{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

{- |
Module      :  Main
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

Программа проверки корректности вычисления интерполяцией (линейной) блеска
для значений атмосферных параметров звезды, находящихся между узлами
исходной сетки.

Во время работы над это программой наблюдалось следующее. Если выполнить
компиляцию с флагом оптимизации -O3, то функция floor3 имеет неожиданное
поведение.  Это приводит к тому, что в функции toArr при инициализации
массива из списка возникает ошибка.  Ошибка связана с тем, что границы
массива определяются некорректно (они уменьшаются).  Что приводит к
видимости того, что в списке для инициализации массива содержатся элементы
вне диапазона.  Проблема исчезает, если компилировать без оптимизации.

-}

module Main where

import Control.Applicative  ( (<$>) )
import Control.Monad.Error  ( MonadError )
import Control.Monad        ( forM_, forM )
import Control.Monad.Reader ( ReaderT, MonadReader, MonadIO, runReaderT, liftIO )
import Data.Array           ( Array, array, inRange, (!) )
import Data.ConfigFile      ( ConfigParser(..) )
import Data.List            ( find, sort )
import Data.Loader.Parser   ( get )
import Data.Loader.Parser.Error ( SCPError(..) )
import Data.Loader          ( getContext )
import Data.NumInstances    ( lift3 )
import Data.HList           ( (:::)(..), Null(..), ToDouble(..), hToList, hMap )
import System.Environment   ( getArgs, getProgName )
import System.Exit          ( exitWith, ExitCode(..) )
import System.FilePath      ( (</>) )
import System.IO            ( Handle, IOMode(..), openFile, hClose, hPutStrLn )
import Synth.Common         ( Spectrum, Teff(..), Logg(..), FeH(..) )
import Synth.Context        ( Context(..), contextParser )
import Synth.Parser         ( Model, parseFluxATLAS9 )
import Synth.Photometry     ( mag )
import Synth.Survey         -- ( GALEX, SDSS, TWOMASS )
import Synth.Survey.GALEX   ()
import Synth.Survey.SDSS    ()
import Synth.Survey.TWOMASS ()
import Text.Printf          ( hPrintf, printf )

import qualified Math.Inter as I

type Point02d = (Double, Double)
type Point03d = (Double, Double, Double)
type Point10d = (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)

type Factory = ReaderT Context IO
type Grid = ((Point03d, Point03d), Point03d)
type Step = Point03d
type Domain = (Point02d, Point02d)
type Parameters = Point03d
type Colors = Point10d

domains :: [Domain]
domains =  
 [ ( (3500, 0.0)
   , (6000, 5.0) )
 , ( (6000, 0.5)
   , (7500, 5.0) )
 , ( (7500, 1.0)
   , (8250, 5.0) )
 , ( (8250, 1.5)
   , (9000, 5.0) )
 , ( (9000, 2.0)
   , (11750, 5.0) )
 , ( (11750, 2.5)
   , (13000, 5.0) )
 , ( (13000, 2.5)
   , (19000, 5.0) )
 , ( (19000, 3.0)
   , (26000, 5.0) )
 , ( (26000, 3.5)
   , (31000, 5.0) )
 , ( (31000, 4.0)
   , (39000, 5.0) )
 , ( (39000, 4.5)
   , (49000, 5.0) )
 ]

main :: IO ()
main = do

 fn <- getArgs >>= parse

 (st, sa,sb,c) <- getContext fn configParser

 runReaderT (process st sa sb) c

parse :: [String] -> IO FilePath
parse argv = case argv of

  [fn] -> return fn
  _    -> usage

 where
    usage = do
      pn <- getProgName
      putStrLn ("Usage : "++ pn ++" CONFIGURATION_FILE") >> exitWith ExitSuccess

process :: (Step,Step) -> [Model] -> [Model] -> Factory ()
process st msa msb = do

   -- Получаем функцию для преобразования спектра в список нужных лесков.
   mf  <- mmf

   -- Получаем функцию вычисления методом интерполяции списка нужных
   -- блесков.  Аргументами функции является астрофизические параметры
   -- звезды.
   imf <- mapM fromModel msa >>= return . toFunc st


   liftIO $ do

     ofh <- openFile "out/fluxinterpol/fluxinterpol.out" WriteMode

     printTableHead ofh

     forM_ msb $ \ ( p, sed ) -> do

       -- Вычисляем блеск, используя спектр.
       let cs = mf sed

       -- вычисляем блеск для тех же значений атмосферных параметром,
       -- используя интерполяцию.
       let (Teff teff ::: Logg logg ::: FeH mh ::: Null) = p

       case imf (teff, logg, mh) of
           Just (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9) -> do
             printTableBody ofh p $ cs ++ [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9]

           Nothing -> do
             print $ "An unexpected error occurred: p = " ++ (show p)

     hClose ofh


-- Функция для отображение спектра в нужный список блесков.
mmf :: Factory ( Spectrum -> [Double] )
mmf = do

  m <- mag :: Factory ( Spectrum ->  FUV GALEX ::: NUV GALEX ::: U SDSS ::: G SDSS ::: R SDSS ::: I SDSS ::: Z SDSS ::: J TWOMASS ::: H TWOMASS ::: K TWOMASS ::: Null)

  return $ (hToList.hMap ToDouble) <$> m

-- Парсер для анализа файла конфигурации.
configParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m ( (Step,Step), [Model], [Model], Context )
configParser = do

  liftIO $ putStrLn  "\nОбработка файла с настройками ...\n"

  sta <- get "ATLAS9_IntGrid" "step13000"
  stb <- get "ATLAS9_IntGrid" "step50000"

  msa <- modelParser "ATLAS9_IntGrid"
  msb <- modelParser "ATLAS9_SubGrid"



  cnt  <- contextParser

  return ((sta,stb), msa, msb, cnt)

-- Парсер для анализа секции
modelParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => String -> m [Model]
modelParser section = do

  ph <- get section "path"
  fn <- get section "file"

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

--
toFunc :: (Step, Step) -> [(Parameters, Colors)] -> Parameters -> Maybe Colors
toFunc st models x = (($ x) . snd) <$> (($ x) . fst) `find`  (fromArrs <$> arrs)
  where
    -- Определяем список массивов. Каждый массив в списке соответствует
    -- одной области из списка указанных областей.
    arrs = toArr st models <$> domains

    -- Вспомогательная функция. 
    fromArrs arr@( ((l,r), _), _ ) = (g, fromArr arr)
      where
        g x = (ok l x) && (ok x r)
          where
            ok (x0,x1,x2) (y0,y1,y2) = x0 <= y0 && x1 <= y1 && x2 <= y2

    -- Определение функции для отображения путем интерполяции массива,
    -- содержащего значения аргумента и значения функции, в непрерывную
    -- функцию.
    fromArr ( ((l,r), h), arr ) = (uncurry3 $ I.triLerp f) . (\ x -> (x-l)/h)
      where
--        f a b c = arr ! (a,b,c)
--{--
        n = floor3 $ (r-l)/h

        f a b c
          | not $ inRange (0,n) i = 0
          | otherwise             = arr ! i
          where
            i = (a,b,c)
--}

--
toArr :: (Step, Step) -> [(Parameters, Colors)] -> Domain -> (Grid, Array (Int,Int,Int) Colors)
toArr (sta,stb) ms (l,r) = ( ((l',r'), h), arr )
  where
    -- Выборка моделей, которые вычислены для указанной области
    -- значений Теff и logg.
    sms = filter (\(x,_) -> (okl x l) && (okr x r)) ms

    -- Определение и сортировка значений [M/H] для отобранных моделей.
    mhs = sort $ (\(_,_,mh) -> mh) <$> fst <$> sms

    -- Определение прямоугольной области значений параметров,
    -- соответствующих отобранным моделям, учитывая значения [M/H].
    l' = (fst l, snd l, head mhs)
    r' = (fst r, snd r, last mhs)

    -- Определение шага изменения парметров.
    h  = if (fst l) < 13000
            then sta -- (250.0, 0.5,0.5)
            else stb -- (1000.0,0.5,0.5)

    -- Опеределение массива из отобранных моделей.
    n  = floor3 $ (r'-l')/h

--    arr = array (0,n) $ (\ (x,y) -> (ceiling3 $ (x-l')/h, y) ) <$> sms
    arr = array (0,n) $ (\ (x,y) -> (floor3 $ (x-l')/h, y) ) <$> sms

    -- Вспомогательные функции для отбора моделей. 
    okl (x0,x1,_) (y0,y1) = x0 >= y0 && x1 >= y1
    okr (x0,x1,_) (y0,y1) = x0 <= y0 && x1 <= y1

--
fromModel :: Model -> Factory (Parameters, Colors)
fromModel (p, s) = do

  -- Получаем функцию для вычисления блеска в нужных обзорах.
  mf <- mmf

  -- Вычисляем блеск, используя модельный спектр.
  let [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = mf s
   
  let (Teff teff ::: Logg logg ::: FeH mh ::: Null) = p

  return ( (teff,logg,mh)
         , (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9) )

-- Далее находятся вспомогательные функции.

floor3 :: (Double, Double, Double) -> (Int, Int, Int)
floor3   = lift3 floor floor floor

ceiling3 :: (Double, Double, Double) -> (Int, Int, Int)
ceiling3 = lift3 ceiling ceiling ceiling

uncurry3 :: (a -> b -> c -> y) -> (a, b, c) -> y
uncurry3 f (a,b,c) = f a b c

printTableHead :: Handle -> IO ()
printTableHead fh = do

  hPutStrLn fh "#"
  hPutStrLn fh "#"
  hPutStrLn fh "#-----------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------+"
  hPutStrLn fh "#     Astrophysical\t|\t\t\t\t\t\t\t      Modelation\t\t\t\t\t\t\t\t\t\t\t|\t\t\t\t\t\t\t     Interpolation\t\t\t\t\t\t\t\t\t\t|"
  hPutStrLn fh "#\t\t\t+-------------------------------+-------------------------------------------------------------------------------+-----------------------------------------------+-------------------------------+-------------------------------------------------------------------------------+---------------------------------------+"
  hPutStrLn fh "# \tparameters\t|            GALEX\t\t|                                SDSS\t\t\t\t\t\t|                     2MASS\t\t\t|             GALEX\t\t|                                SDSS\t\t\t\t\t\t|             2MASS\t\t\t| "
  hPutStrLn fh "#-----------------------+-------------------------------+-------------------------------------------------------------------------------+-----------------------------------------------+-------------------------------+-------------------------------------------------------------------------------+---------------------------------------+"
  hPutStrLn fh "#Teff\tlogg\t[M/H]\t|FUV\t\tNUV\t\t|u\t\tg\t\tr\t\ti\t\tz\t\t|J\t\tH\t\tK\t\t|FUV\t\tNUV\t\t|u\t\tg\t\tr\t\ti\t\tz\t\t|J\t\tH\t\tK\t|"
  hPutStrLn fh "#-----------------------+-------------------------------+-------------------------------------------------------------------------------+-----------------------------------------------+-------------------------------+-------------------------------------------------------------------------------+---------------------------------------+"

printTableBody :: Handle -> Teff ::: Logg ::: FeH ::: Null -> [Double] -> IO () 
printTableBody fh p cs = do

   let (Teff t ::: Logg g ::: FeH feh ::: Null) = p

   let ls = printf "%f\t%.3f\t%.3f\t%s\n" t g feh (ss :: String)

   hPrintf fh "%s" (ls::String)

   printf "%s" (ls::String)

   where
      ss = (printf "%8.4f\t") `concatMap` cs