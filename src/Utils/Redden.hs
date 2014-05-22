{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Main
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Main where

import Control.Applicative  ( (<$>) )
import Data.HList           ( (:::)(..), Null(..) )
import System.Environment   ( getArgs, getProgName )
import System.Exit          ( exitWith, ExitCode(..) )
import Synth.Common         ( Spectrum, Av(..), Rv(..) )
import Synth.Parser         ( parseFluxATLAS9 )
import Text.Printf          ( printf )

import qualified Synth.Extinction.CCM as IE

main :: IO ()
main = do

 (fn, ar) <- getArgs >>= parse

 sed <- parseFluxATLAS9 fn

 rsed <- sed `redden` ar

 mapM_ (\ (x,y) -> printf "%f\t%f\n" x y) rsed


redden :: Spectrum -> (Av ::: Rv ::: Null) -> IO (Spectrum)
redden sed ar = do
  
  ie <- IE.model

  return $ (\ (w, f) -> (w, f * (ie ar w)) ) <$> sed

parse :: [String] -> IO (FilePath, (Av ::: Rv ::: Null))
parse argv = case argv of

  fn : x : y : _  -> case (reads x, reads y) of
                         ( [(av,_)], [(rv,_)] ) -> return (fn, Av av ::: Rv rv ::: Null)
                         _                      -> usage
  _               -> usage

 where
    usage = do
      pn <- getProgName
      putStrLn ("Usage : "++ pn ++" PATH_TO_FLUX_ATLAS9_FILE Av Rv") >> exitWith ExitSuccess