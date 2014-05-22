{-# OPTIONS -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Main
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Main where

import Control.Monad.Reader ( runReaderT )
import Data.Loader          ( getContext )
import System.Environment   ( getArgs, getProgName )
import System.Exit          ( exitWith, ExitCode(..) )
import Synth.Context        ( configParser )
import Synth.Synth          ( process )

main :: IO ()
main = do

 fn <- getArgs >>= parse

 (o,m,c) <- getContext fn configParser

 runReaderT (process o m) c

parse :: [String] -> IO FilePath
parse argv = case argv of

  [fn] -> return fn
  _    -> usage

 where
    usage = do
      pn <- getProgName
      putStrLn ("Usage : "++ pn ++" CONFIGURATION_FILE") >> exitWith ExitSuccess
