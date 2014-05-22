module Data.Loader.Parser.Error (
   SCPError(..),
 ) where

import Control.Monad.Error

import qualified Data.ConfigFile    as C
import qualified Data.Loader.TableParser as T

data SCPError =   CPError FilePath C.CPError
                | TPError FilePath T.TPError
                deriving Show

instance Error SCPError where
    noMsg    = CPError "" noMsg
    strMsg x = CPError "" (strMsg x)

{-
  Нужно добавить функции красивого и понятного преобразования SCPError в String. 
-}