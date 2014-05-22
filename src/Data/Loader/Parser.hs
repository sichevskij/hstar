{-# LANGUAGE TypeOperators, FlexibleContexts #-}
{- |
Module      :  Data.Loader.Parser
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Data.Loader.Parser
  ( get
  , parseConfigFile
  , parseTableFile
  , parseArrayFile
  )
    where

import Control.Monad.Reader         ( ReaderT, MonadReader, MonadIO, runReaderT, asks, join, liftIO)
import Control.Monad.Error          ( MonadError, throwError, strMsg )
import Control.Monad.List           ( ListT(..), runListT )

import Data.Array                   ( Array, array )
import Data.ConfigFile              ( emptyCP, readfile, ConfigParser(..), SectionSpec, OptionSpec, Get_C )

import Data.Loader.Parser.Error       ( SCPError(..) )
import Data.Loader.TableParser hiding ( get )

import System.IO                    ( hPutStr, stderr )
import Text.Scanf                   ( Parser )

--import Types

import qualified Data.Text.IO       as TIO
import qualified Data.ConfigFile    as CF
import qualified Data.Loader.TableParser as T


-- |Анализатор файла конфигурации. Файл конфигурации - это  INI-подобный файл,
-- но с некоторым количеством улучшений.
parseConfigFile :: (MonadError SCPError m, MonadIO m) => FilePath -> ReaderT (ConfigParser, FilePath) m a -> m a
parseConfigFile fn p = do
   let cp = emptyCP {optionxform = map id}
   cp' <- join $ liftIO $ readfile cp fn >>= return . either (throwError . CPError fn) (return)
   runReaderT p (cp',fn)

-- |Извлекает информацию из строки в конфигурационном файле.
-- Может применяться в парсере анализатора parseConfigFile.
get :: (Show a, Get_C a, MonadReader (ConfigParser, FilePath) m, MonadError SCPError m, MonadIO m) => SectionSpec -> OptionSpec -> m a
get s o = do
  cp <- asks $ fst
  fn <- asks $ snd
  liftIO $ hPutStr stderr $ s ++"::"++ o ++ " = "
  v <- either (throwError . CPError fn) (return) $ CF.get cp s o
  liftIO $ hPutStr stderr $ (show v) ++ "\n"
  return v

-- |Анализатор файла с данными представленными в виде таблицы, в котором имеется
-- описательная (конфигурационная) часть, расположенная перед таблицей.
parseTableFile :: (MonadIO m, MonadError SCPError m) => ReaderT TableConfigParser (Either TPError) (Parser a) -> ((ConfigParser, [a]) -> Either TPError b) -> FilePath -> m b
parseTableFile contentParser postParser fname = do
   s <- liftIO $ TIO.readFile fname
   let m = readTableWithConfig s contentParser >>= postParser
   either (throwError.TPError fname) (return) m

-- |Анализатор, построенный на основе parseTableFile. Извлекает информацию
-- из файла и преобразует ее в 2-ух мерный массив. Количество элементов
-- массива определяется в конфигурационной части анализируемого файла.
parseArrayFile :: (MonadIO m, MonadError SCPError m) => ReaderT TableConfigParser (Either TPError) (Parser [e]) -> FilePath -> m (Array (Int, Int) e)
parseArrayFile contentParser = parseTableFile contentParser postParser
   where
      postParser (cp,t) = do
           (l,r) <- T.get cp "DEFAULT" "Bounds"

           case (indexS l r t) of
                 Just a  -> return $ array (l,r) a
                 Nothing -> throwError $ strMsg "It is epic fail!!!"
         where
            indexS :: (Int,Int) -> (Int,Int)-> [[a]] -> Maybe [((Int,Int),a)]
            indexS (l,l') (r,r') xs  = runListT $ do
                                                    (i, ys) <- ListT $ zipS [l .. r ] xs
                                                    (j,  x) <- ListT $ zipS [l'.. r'] ys
                                                    return ((i,j), x)

            zipS :: [a] -> [b] -> Maybe [(a,b)]
            zipS [] [] = Just []
            zipS (x:xs) (y:ys) = do
                                   r <- zipS xs ys
                                   return $ (x,y) : r
            zipS _ _ = Nothing
