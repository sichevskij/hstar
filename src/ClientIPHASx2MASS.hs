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
import Data.Object              ( IPHASxTWOMASS(..))
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
toObj :: [Float] -> (Point06d, IPHASxTWOMASS)
toObj (teff:logg:mh:av:rv:th: _:_: _:_:_:_:_: x7:x8:x9: x10:x11:x12:_) = (p, o)
  where
    p = (teff,logg,mh,av,rv,th)
    o = IPHASxTWOMASS (x7,x8,x9, x10,x11,x12)

-- |
extraTaskHandler:: (NumId, (Title, Req.Object)) -> Maybe [(Point06d, Float, IPHASxTWOMASS)] ->  ReaderT (Env IPHASxTWOMASS) IO ()
extraTaskHandler (_, (_, obj)) (Just pdo) = extraTaskHandler' obj
  where
     extraTaskHandler' (OxIPHASxTWOMASS o' _) = do

      hRes <- asks $ hSample

      forM_ pdo $ \ (p, d, o) -> do

        let (x0,x1,x2,x3,x4,x5) = p
        let IPHASxTWOMASS (j, h, k,  r, i, hα ) =       o'  -       o
        let IPHASxTWOMASS (j',h',k', r',i',hα') = (proj o') - (proj o)

        liftIO $ do
          hPrintf hRes "%8.2f %8.4f %8.4f %8.4f %8.4f %8.6f %10.7f " x0 x1 x2 x3 x4 x5 d
          hPrintf hRes "%8.4f %8.4f %8.4f %8.4f %8.4f %8.4f "  j  h  k  r  i  hα
          hPrintf hRes "%8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n" j' h' k' r' i' hα'

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

         teff <- string
         logg <- string
         mh   <- string
         av   <- string
         rv   <- string
         th   <- string

         -- 2MASS
         j  <- float
         h  <- float
         k  <- float
         -- IPHAS
         r  <- float
         i  <- float
         hα <- float

         let title = printf "%s %s %s %s %s %s" teff logg mh av rv th
             obj   = IPHASxTWOMASS (j,h,k, r, i, hα)
             unc   = (0.034,0.041,0.045, 0.03, 0.03, 0.03)

         return (title, OxIPHASxTWOMASS obj unc)
