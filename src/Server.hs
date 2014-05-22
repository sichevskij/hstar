{-# OPTIONS -Wall #-}

{- |
Module      :  Main
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

import Control.Concurrent   ( forkIO )
import Control.Monad        ( forever )
import Control.Monad.Reader ( runReaderT )
import Data.Loader          ( getContext )
import System.Environment   ( getArgs, getProgName )
import System.Exit          ( exitWith, ExitCode(..) )
import System.IO            ( hSetBuffering, BufferMode(..), hClose )
import Network              ( listenOn, withSocketsDo, accept, PortID(..) )
import Server.Context       ( configParser )
import Server.Server        ( reqHandler )

main :: IO ()
main = withSocketsDo $ do
  -- Из аргументов программы получаем полный путь к конфигурационному файлу
  -- и номер порта.
  (fn, port) <- getArgs >>= parse
  -- Считывание данных из фала конфигураци.
  cont <- getContext fn configParser

  putStrLn $ "\nСервер запущен и ждет подключения клиентов."

  socket <- listenOn port
  -- We want to accept multiple connections,
  -- so we need to accept in a loop
  forever $ do 
      (handle, host, portno) <- accept socket
      -- But once we've accepted, we want to go off and handle the
      -- connection in a separate thread
      forkIO $ do
          hSetBuffering handle LineBuffering
          runReaderT (reqHandler handle) cont
          hClose handle

-- | Анализ аргументов программы для определения полного пути к
-- конфигурационному файлу и номера порта для соединения.
parse :: [String] -> IO (String, PortID)
parse argv = case argv of

  fn : p : _ -> case reads p of
                      (pn, _) : _ -> return ( fn, PortNumber $ fromInteger pn )
                      _           -> usage
  _          -> usage

 where
    usage = do
      pn <- getProgName
      putStrLn ("Usage : "++ pn ++" CONFIG_FILE PORT") >> exitWith ExitSuccess