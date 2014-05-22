{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface #-}

{- |
Module      :  Server.Context
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Server.Context
  ( Context(..)
  , Tree(..)
  , configParser
  )
    where

--import Control.Applicative      ( (<$>) )
import Control.Monad.Error      ( MonadError )
import Control.Monad.Reader     ( MonadReader, MonadIO, liftIO )
import Data.ConfigFile          ( ConfigParser(..) )
import Data.Loader.Parser       ( get )
import Data.Loader.Parser.Error ( SCPError(..) )
--import Data.Point               ( Point05d )
import Data.Time.Clock          ( getCurrentTime, utctDayTime )
import Foreign                  ( Ptr )
import Foreign.C.String         ( CString, newCString )
import Text.Printf              ( printf )
import GSL.Random.Gen           ( newRNG, mt19937 )

import qualified Data.Object as O

-- import the foreign function as normal
foreign import ccall "load_kdtree"
  loadtree :: CString -> IO (Ptr Tree)

--
data ObjDist = Normal | Uniform deriving (Read, Show)
--
data Tree = Tree

--
data Context = Context
  { kdtree_galex :: Maybe (Ptr Tree)
  , kdtree_sdss  :: Maybe (Ptr Tree)
  , kdtree_twomass :: Maybe (Ptr Tree)
  , kdtree_gri_twomass :: Maybe (Ptr Tree)
  , kdtree_griz_twomass :: Maybe (Ptr Tree)
  , kdtree_sdss_twomass :: Maybe (Ptr Tree)
  , kdtree_nuv_sdss_twomass :: Maybe (Ptr Tree)
--  , database   :: Ptr Point05d
  , serverName :: String
  , rndObjDist :: O.ObjDist
  , numRndObjs :: Int
  }

-- | Парсер для анализа файла конфигурации.
configParser :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => m ( Context  )
configParser = do

  liftIO $ putStrLn  "\nОбработка файла с настройками сервера...\n"

  sn <- get "OPTIONS" "ServerName"
  od <- get "OPTIONS" "RndObjDist"
  no <- get "OPTIONS" "NumRndObjs"

--  db <- get "DATABASE" "DataFile" >>= parseDataFile
  galex <- get "DATAINDEX" "GALEX" >>= parseIndexFile
  sdss  <- get "DATAINDEX" "SDSS" >>= parseIndexFile
  twomass <- get "DATAINDEX" "TWOMASS" >>= parseIndexFile
  gri_twomass <- get "DATAINDEX" "GRIxTWOMASS" >>= parseIndexFile
  griz_twomass <- get "DATAINDEX" "GRIZxTWOMASS" >>= parseIndexFile
  sdss_twomass <- get "DATAINDEX" "SDSSxTWOMASS" >>= parseIndexFile
  nuv_sdss_twomass <- get "DATAINDEX" "NUVxSDSSxTWOMASS" >>= parseIndexFile

  rd <- liftIO $ do
          case od of
            Uniform -> return   O.Uniform
            Normal  -> return . O.Normal =<< newRNG mt19937

  return $ Context { serverName = sn
                   , rndObjDist = rd
                   , numRndObjs = no
--                   , database = db
                   , kdtree_galex = galex
                   , kdtree_sdss = sdss
                   , kdtree_twomass = twomass
                   , kdtree_gri_twomass = gri_twomass
                   , kdtree_griz_twomass = griz_twomass
                   , kdtree_sdss_twomass = sdss_twomass
                   , kdtree_nuv_sdss_twomass = nuv_sdss_twomass
                   }
{--
-- | Парсер для анализа раздела файла конфигурации
parseDataFile :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => FilePath -> m ( Ptr Point05d )
parseDataFile fn = liftIO $ do
      -- Считываем содержимое файла. Затем разбиваем его на строки,
      -- пропустив строки, начинающиеся с '#', строки разбиваем на слова,
      -- оставив только пять первых слова. A из слов считываем числа.
      newArray . (( \[a,b,c,d,e] -> (a,b,c,d,e)) <$>) . ((read <$>) . (take 5 . words) <$>) . (filter ((/='#').head) . lines) =<< readFile fn
--}

-- | Парсер для анализа раздела файла конфигурации
parseIndexFile :: (MonadReader (ConfigParser, FilePath) m , MonadError SCPError m , MonadIO m) => Maybe FilePath -> m ( Maybe (Ptr Tree) )
parseIndexFile (Just fn) = liftIO $ do

   stime <- getCurrentTime >>= return . utctDayTime
   printf "Загрузка индекс-файла '%s'...\n" fn

   kdtree <- newCString fn >>= loadtree

   etime <- getCurrentTime >>= return . utctDayTime
   printf "Время загрузки файла : %s sec\n" (show $ etime - stime)

   return $ Just kdtree

parseIndexFile Nothing   = return Nothing