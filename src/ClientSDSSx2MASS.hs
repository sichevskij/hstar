{-# OPTIONS -fno-warn-missing-fields -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

{- |
Module      :  Main
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

import Client.Client            ( waitCmd, taskHandler, parseArgs, Env(..), NumId, Name, Title )
import Client.Context           ( configParser )
import Control.Monad            ( forM, forM_ )
import Control.Monad.Error      ( MonadError )
import Control.Monad.Reader     ( MonadReader, ReaderT, MonadIO, asks, liftIO, lift )
import Data.ConfigFile          ( ConfigParser(..) )
import Data.Object              ( SDSSxTWOMASS(..))
import Data.Point               ( Point06d )
import Data.Loader              ( runParser )
import Data.Loader.Parser       ( get, parseTableFile )
import Data.Loader.Parser.Error ( SCPError(..) )
import Math.Proj                ( proj )
import Server.Request           ( Object(..) )
import System.Environment       ( getArgs )
import System.FilePath.Posix    ( (</>), (<.>) )
import System.IO                ( hSetBuffering,  hPutStrLn, hClose, stderr,  BufferMode(..) )
import Text.Printf              ( printf, hPrintf )
import Text.Scanf               ( skip, float, decimal, string )
import Network                  ( connectTo, withSocketsDo )

import qualified Server.Request as Req


main :: IO ()
main = withSocketsDo $ do
  -- Из аргументов программы получаем полный путь к конфигурационному файлу
  -- и номер порта, к которому нужно подсоединиться.
  (fn, host, port) <- getArgs >>= parseArgs

  -- Считывание данных из фала конфигурации и файла, содержащего входные
  -- данные, то есть фотометрию звезд.
  cnt <- runParser fn $    configParser toObj
  nos <- runParser fn $ inputFileParser

  -- Подсоединяемая к заданному порту и устанавливаем тип буферизации.
  handle <- connectTo host port
  hSetBuffering handle LineBuffering

  putStrLn "\nКлиент запущен и подключен к серверу."

  -- Определяем окружение для выполнения команд.
  let env = Env { context = cnt
                , tasks   = nos
                , hServer = handle
                , respHandler = \ o r -> extraTaskHandler o =<< taskHandler o r
                }
  -- Ждем поступления команды из стандартного входа программы и пытаемся ее
  -- исполнить.
  waitCmd env

  hClose handle


-- |
toObj :: [Float] -> (Point06d, SDSSxTWOMASS)
toObj (teff:logg:mh:av:rv:th: _:_:x2:x3:x4:x5:x6:x7:x8:x9:_) = (p, o)
  where
    p = (teff,logg,mh,av,rv,th)
    o = SDSSxTWOMASS (x2,x3,x4,x5,x6, x7,x8,x9)

-- |
extraTaskHandler:: (NumId, (Title, Req.Object)) -> Maybe [(Point06d, Float, SDSSxTWOMASS)] ->  ReaderT (Env SDSSxTWOMASS) IO ()
extraTaskHandler (_, (_, obj)) (Just pdo) = extraTaskHandler' obj
  where
     extraTaskHandler' (OxSDSSxTWOMASS o' _) = do

      hRes <- asks $ hSample

      forM_ pdo $ \ (p, d, o) -> do

        let (x0,x1,x2,x3,x4,x5) = p
        let SDSSxTWOMASS (u, g, r, i, z,  j, h, k ) =       o'  -       o
        let SDSSxTWOMASS (u',g',r',i',z', j',h',k') = (proj o') - (proj o)

        liftIO $ do
          hPrintf hRes "%8.2f %8.4f %8.4f %8.4f %8.4f %8.6f %10.7f " x0 x1 x2 x3 x4 x5 d
          hPrintf hRes "%8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f "  u  g  r  i  z  j  h  k
          hPrintf hRes "%8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n" u' g' r' i' z' j' h' k'

--     extraTaskHandler' _ = return () -- Здесть нужно выводить сообщение, что 


extraTaskHandler _ Nothing = return ()

-- | Парсер для анализа файла.
inputFileParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m [ (Name, [(Title, Req.Object)]) ]
inputFileParser = do

  liftIO $ hPutStrLn stderr "\nParsing input file...\n"

  ws  <- get "INPUT"  "Names"
  dir <- get "INPUT"  "Location"

  forM (words ws) $ \ name -> do

    objs <- parseInputFile $ dir </> name <.> "txt"

    return (name, objs)

-- |
parseInputFile :: (MonadError SCPError m, MonadIO m) => FilePath -> m [(Title, Req.Object)]
parseInputFile = parseTableFile cp (return.snd)
  where
    cp = lift $ return $ do
         --
         objid <- decimal
         --
         plate <- decimal
         mjd <- decimal
         fiberid <- decimal
         --
         skip; skip -- l <- string; b <- string
         --
         teff  <- string; teffe <- string
         logg  <- string; logge <- string
         feh   <- string; fehe  <- string
         -- SDSS
         u  <- float; ue <- float
         g  <- float; ge <- float
         r  <- float; re <- float
         i  <- float; ie <- float
         z  <- float; ze <- float
         -- 2MASS
         j  <- float
         h  <- float
         k  <- float

         let title = printf "%d spec-%04d-%04d-%04d %s %s %s %s %s %s" objid plate mjd fiberid teff teffe logg logge feh fehe
             obj   = SDSSxTWOMASS (u,g,r,i,z, j,h,k)
             err   = (  ue,   ge,   re   ,ie,  ze,  0.01, 0.01, 0.01)
             unc   = (0.06,0.045,0.045,0.045,0.06, 0.034,0.041,0.045)

         return (title, OxSDSSxTWOMASS obj (sqrt $ err**2 + unc**2) )
