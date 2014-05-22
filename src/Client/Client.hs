{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

{- |
Module      :  Client.Client
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Client.Client
  ( waitCmd
  , taskHandler
  , parseArgs
  , Env(..)
  , NumId
  , Name
  , Title
  )
    where

import Control.Applicative    ( (<$>) )
import Control.Monad          ( forM_, forM )
import Control.Monad.Reader   ( runReaderT, withReaderT, ReaderT, asks, liftIO )
import Client.Context         ( Context(..) )
import Data.Array.IO          ( readArray )
import Data.Vector            ( fromList )
import Data.Point             ( Point06d )
import  Data.Word              ( Word64 )
import Data.Serialize         ( decode )
--import Data.Binary            ( decode )
import Server.Response        ( Response(..), OkContent(..) )
import Server.Request         ( Request(..), Cmd(..) )
import Statistics.Sample      ( meanVarianceUnb )
import System.Environment     ( getProgName )
import System.Exit            ( exitWith, ExitCode(..) )
import System.FilePath.Posix  ( (</>), (<.>) )
import System.Posix.Directory ( createDirectory )
import System.Directory       ( doesFileExist )
import System.IO              ( hPutStrLn, hClose, openFile, IOMode(..), Handle )
import System.IO.Error        ( catchIOError, isAlreadyExistsError,)
import Text.Printf            ( printf, hPrintf )
import Network                ( PortID(..) )
import GHC.Float              ( float2Double )

import qualified Data.ByteString.Char8 as B
import qualified Server.Request        as Req

type NumId = String
type Name  = String
type Title = String
type Task  = (Name, [(Title, Req.Object)])

data Env c = Env { context :: Context c
                 , tasks   :: [Task]
                 , hServer :: Handle
                 , hOutput :: Handle
                 , hSample :: Handle
                 , hLoggin :: Handle
                 , respHandler :: (NumId, (Title, Req.Object)) -> Maybe [(Int,Float)] -> ReaderT (Env c) IO ()
                 }

-- Функция, ожидающая поступления из стандартного входа программы команды и
-- пытающаяся ее исполнить.
waitCmd :: Env c -> IO ()
waitCmd env = do
  cmd <- getLine
  -- Получаем команду в виде строки из стандартного входа. Проверяем
  -- является полученная команда командой для завершения работы клиента.
  if cmd == "quit"
     then do
            -- Если полученная команда - это команда выхода, то выводим
            -- сообщение о закрытии соединения с сервером.
            putStrLn "The client was disconnected!"
     else do
            -- Иначе исполняем команду и переходим в ожидание следующей
            -- команды.
            exeCmd cmd
            waitCmd env
  where
    exeCmd cmd = do
      -- В зависимости от команды выполняются соответствующие действия.
      case cmd of
        "begin a job" -> do
                           -- Запуск в заданном контексте задания на
                           -- обработку списка, содержащего фотометрию.
                           runReaderT beginJob env
        _             -> do
                           -- В случае недефферинцируемой команды, она
                           -- полностью передается на сервер, а ответ
                           -- выводится в стандартный вывод программы.
--                           hPutStrLn (hServer env) cmd >> B.hGetLine (hServer env) >>= putStrLn <$> show.decode
                           hPutStrLn (hServer env) cmd >> hGetResp (hServer env) >>= putStrLn <$> show

-- | Функция формирующая и передающая на сервер команды для обработки
-- объектов с фотометрией.
beginJob :: ReaderT (Env c) IO ()
beginJob = do
  -- Запрашиваем из контекста выполнения задания путь к директории для
  -- записи результатов, список заданий (т.е.  перечень фотометрии), и
  -- файловый дескриптор для связи с сервером.
  dir  <- asks $ outDir . context
  nos  <- asks $ tasks
  hOut <- asks $ hServer

  forM_ nos $ \ (name, objs) -> do
      -- Создаем в директории для записи результатов директория с названием
      -- задачи.  Директория создается только если ее нет.
      liftIO $ createDirectoryIfMissing $ dir </> name

      k <- numSkipObjs name
      -- Определяем количество ранее обработанной объектов. Нумеруем объекты
      -- и пропускаем обработанные.
      forM_ ( drop k $ zipWith (,) nums objs ) $  \ (num, obj) -> do
          -- Для каждого объекта формируем команду и оправляем ее на сервер.
          -- После оправки команды переходим в ожидание ответа.
          let cmd = show ( Req $ Task $ snd obj )

          liftIO $ hPutStrLn hOut cmd

          waitResp name (num,obj)

--          testWaitResp num
  where
    -- Список для нумерации объектов.
    nums = printf "%06d" <$> [1::Int ..]

{--
testWaitResp :: NumId -> ReaderT (Env c) IO ()
testWaitResp num = do
  
  hIn  <- asks hServer

  resp <- liftIO $ hGetResp hIn

  liftIO $ putStrLn $ show $ length $ show resp

  case resp of
    Right( OkResp _ (TaskResultResp  _) ) -> return ()
    _                                     -> testWaitResp num
--}

-- | Функция для ожидания ответа от сервера и передачи его в обрабатывающую
-- функцию.
waitResp ::  Name -> (NumId, (Title, Req.Object)) -> ReaderT (Env c) IO ()
waitResp name (num, obj) = do
  -- Запрашиваем из контекста выполнения задания путь к директории для
  -- записи результатов.
   dir   <- asks $ outDir . context

   -- Создаем директорию с номером задачи.
   liftIO $ createDirectoryIfMissing $ dir </> name </> num 

   hLog <- liftIO $ openFile (dir </> name </> num </> "message" <.> "log")  AppendMode
   hRes <- liftIO $ openFile (dir </> name </> num </> "sample"  <.> "txt")  WriteMode
   hOut <- liftIO $ openFile (dir </> name <.> "txt") AppendMode

   withReaderT (setHandler hLog hRes hOut) $ do waitTaskResult

   liftIO $ do
     hClose hLog
     hClose hRes
     hClose hOut

   where
     --
     setHandler hLog hRes hOut env = env { hOutput = hOut, hSample = hRes, hLoggin = hLog }

     --
     waitTaskResult = do

        hIn   <- asks hServer
        hLog  <- asks hLoggin
        taskH <- asks respHandler

        resp <- liftIO $ hGetResp hIn

        case resp of
          Right ( OkResp _ (TaskResultResp  r) ) -> do
                                                      taskH (num, obj) r
          Right ( OkResp{} )                     -> do
                                                      hPutLog hLog $ show resp
                                                      waitTaskResult
          _                                      -> do
                                                      hPutLog hLog $ show resp


-- |
taskHandler :: (NumId, (Title, Req.Object)) -> Maybe [(Int,Float)] ->  ReaderT (Env c) IO (Maybe [(Point06d, Float, c)])
taskHandler (num, (title, _)) (Just resp) = do

  db   <- asks $ dataBase . context
  hOut <- asks $ hOutput

  liftIO $ do

    pdo <- forM resp $ \ (ix, d) -> do
                                      (p,o) <- readArray db ix

                                      return (p,d,o)
--{--
    let xs = (\ ((x0,x1,x2,x3,x4,x5), d, _) -> float2Double <$> [x0,x1,x2,x3,x4,x5, d]) <$> pdo

        (teff, vTeff) = meanVarianceUnb $ fromList $ (!!0) <$> xs
        (logg, vLogg) = meanVarianceUnb $ fromList $ (!!1) <$> xs
        (mh,   vMH)   = meanVarianceUnb $ fromList $ (!!2) <$> xs
        (av,   vAv)   = meanVarianceUnb $ fromList $ (!!3) <$> xs
        (rv,   vRv)   = meanVarianceUnb $ fromList $ (!!4) <$> xs
        (th,   vTh)   = meanVarianceUnb $ fromList $ (!!5) <$> xs
        (ds,   vDs)   = meanVarianceUnb $ fromList $ (!!6) <$> xs
   
    hPrintf hOut "%s %s " num title
    hPrintf hOut "%8.2f %8.2f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %8.6f %8.6f %8.5f %8.5f\n" teff (vTeff**0.5) logg (vLogg**0.5) mh (vMH**0.5) av (vAv**0.5) rv (vRv**0.5) th (vTh**0.5) ds vDs
--}
    return $ Just pdo

taskHandler _ Nothing = return Nothing

-- |
--{--
hGetResp :: Handle -> IO (Either String Response)
hGetResp h = do

  n <- getRespLength

  case n of
    Right num -> do
                   decode <$> B.hGet h (fromIntegral num)

    Left err  -> do 
                   return $ Left ("An error occurred while trying to get the response length: " ++ err)
  where
    getRespLength :: IO (Either String Word64)
    getRespLength =  decode <$> B.hGet h 8
--}
{--
hGetResp :: Handle -> IO Response
hGetResp h = do

  n <- getRespLength

  decode <$> B.hGet h (fromIntegral n)

  where
    getRespLength :: IO Word64
    getRespLength =  decode <$> B.hGet h 8
--}

-- |
numSkipObjs :: String -> ReaderT (Env c) IO Int
numSkipObjs name = do

  dir <- asks $ outDir . context

  liftIO $ do

    let fname = dir </> name <.> "txt"

    ok <- doesFileExist fname

    n <- if ok
          then do 
                 cs <- readFile fname
                 let ns = (read . head . words) <$> lines cs 
                 return $ if null ns then 0 else last ns
          else do
                 return 0
    return n

-- | Анализ аргументов программы для определения полного пути к
-- конфигурационному файлу и номера порта для соединения.
parseArgs :: [String] -> IO (String, PortID)
parseArgs argv = case argv of

  fn : p : _ -> case reads p of
                      (pn, _) : _ -> return ( fn, PortNumber $ fromInteger pn )
                      _           -> usage
  _            -> usage

 where
    usage = do
      pn <- getProgName
      putStrLn ("Usage : "++ pn ++" PATH_CONFIGURATION_FILE PORT") >> exitWith ExitSuccess

-- |
createDirectoryIfMissing :: FilePath -> IO ()
createDirectoryIfMissing dir = do
  catchIOError (createDirectory dir 0o755) $ \e -> if isAlreadyExistsError e then return () else ioError e

hPutLog :: Handle -> String -> ReaderT (Env c) IO ()
hPutLog h msg = liftIO $ do
  putStrLn msg >> hPutStrLn h msg
