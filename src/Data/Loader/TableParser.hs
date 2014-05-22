{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
{- |
Module      :  Data.Loader.TableParser
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2012
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable


-}

module Data.Loader.TableParser
 ( TableConfigParser
 , TPError
 , Get (..)
 , readTableWithConfig
 )
   where

import Control.Monad.Error
import Control.Monad.Reader

import Data.ConfigFile ( ConfigParser(..), CPError, SectionSpec, OptionSpec, readstring, emptyCP )
import Data.Typeable   ( Typeable, typeOf )

import qualified Data.Text       as T
import qualified Text.Scanf      as S
import qualified Data.ConfigFile as CF

type TableConfigParser = ConfigParser
data TPError           = CPError CPError | ParseError S.ParserError deriving Show

instance Error TPError where
    noMsg    = CPError $ noMsg
    strMsg x = CPError $ strMsg x

class Get a where
   get :: ConfigParser -> SectionSpec -> OptionSpec ->  Either TPError a

instance Get String where
   get cp s = liftL . CF.get cp s

instance (Read a, Typeable a) => Get a where
   get = getGeneric undefined

getGeneric :: (Read a, Typeable a) => a -> ConfigParser -> SectionSpec -> OptionSpec -> Either TPError a
getGeneric r cp s o = do
   val <- get cp s o
   case reads val of
      (v,""):_ -> return v
      _        -> liftL $ throwError (CF.ParseError $ "couldn't parse type " ++ (show $ typeOf r) ++ " " ++ val ++ " from " ++ "(" ++ s ++ "/" ++ o ++ ")", "get")

{--
get :: (Get_C a) => ConfigParser -> SectionSpec -> OptionSpec ->  Either TPError a
get cp s = liftL . CF.get cp s
--}

liftL :: Either CPError a -> Either TPError a
liftL = either (throwError.CPError) (return)

liftR :: Either S.ParserError a -> Either TPError a
liftR = either (throwError.ParseError) (return)

readTableWithConfig :: T.Text -> ReaderT TableConfigParser (Either TPError) (S.Parser a) -> Either TPError (ConfigParser, [a]) 
readTableWithConfig s m = do
  let (header, rows) = splitTable s
  cp      <- liftL $ readstring emptyCP {optionxform = map id} (T.unpack header)
  tparser <- runReaderT m cp
  r       <- liftR $ S.parseRows tparser rows
  return (cp, r)

splitTable :: T.Text -> (T.Text, [T.Text])
splitTable t = (header, rows)
   where
     header = T.concat $ map ((\x -> T.snoc x '\n' ) . T.dropWhile (\x -> x == ' ' || x == '\t') . T.tail) xs

     rows   = filter (\l -> not (T.null l) && T.head l /= '#' ) ys

     (xs, ys) = span g $ T.lines t

     g x | T.null x  = False
         | otherwise = T.head x == '#'
