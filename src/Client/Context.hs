{-# LANGUAGE FlexibleContexts #-}
{- |
Module      :  Client.Context
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Client.Context
  ( Context(..)
  , configParser
  )
    where

import Control.Applicative      ( (<$>) )
import Control.Monad.Error      ( MonadError )
import Control.Monad.Reader     ( MonadReader, MonadIO, liftIO )
import Data.Array.IO            ( IOArray, newListArray )
import Data.ConfigFile          ( ConfigParser(..) )
import Data.Loader.Parser       ( get )
import Data.Loader.Parser.Error ( SCPError(..) )
import Data.Point               ( Point06d )
import Data.Maybe               ( fromJust )
import System.IO                ( hPutStrLn, stderr )
import GHC.Float                ( double2Float )

import qualified Data.ByteString.Char8      as LB
import qualified Data.ByteString.Lex.Double as LD

--
data Context c = Context
  { outDir   :: FilePath
  , dataBase :: IOArray Int (Point06d, c)
  }

-- | Парсер для анализа файла конфигурации.
configParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => ( [Float] -> (Point06d, c) ) ->  m (Context c)
configParser toObj = do

  liftIO $ hPutStrLn stderr "\nParsing configuration file...\n"

  oD <- get "OUTPUT" "Location"

  db <- get "DATABASE" "DataFile" >>= parseDataFile toObj

  return $ Context
    { outDir = oD
    , dataBase = db
    }

-- | Парсер для анализа раздела файла конфигурации
parseDataFile :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => ( [Float] -> (Point06d, c) ) -> FilePath -> m ( IOArray Int (Point06d, c) )
parseDataFile toObj fn = liftIO $ do
  -- Считываем содержимое файла.
  cs <- LB.readFile fn

  -- Затем разбиваем его на строки, пропустив первую строку и
  -- последующие комментарии.
  let lls =  (filter ((/='#') . LB.head) . tail . LB.lines) cs

  -- Строки разбиваем на слова.
  let lws = filter (not.null) $ LB.words <$> lls

  -- Из слов считываем числа. В конце выполняем конвертацию из списка в
  -- нужный объект при помощи заданной функции. 
  let objs = toObj <$> (readFloat <$>) <$> lws

      n = length( objs ) - 1

  newListArray (0,n) objs
  
  where
    readFloat = double2Float . fst . fromJust . LD.readDouble
