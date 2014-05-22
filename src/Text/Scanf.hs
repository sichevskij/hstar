{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- |
Module      :  Text.Scanf
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Lebedev Alexander 2012
License     :  BSD3

Maintainer  :  lebastr@gmail.com
Stability   :  experimental
Portability :  portable


-}

module Text.Scanf where

import Control.Monad
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State.Strict

import GHC.Float ( double2Float )

import qualified Data.Text      as T
import qualified Data.Text.Read as R

type Column = Int
type Row = Int

data ParserError = ParserError { row :: Row
                               , col :: Maybe Column
                               , message :: String } deriving (Show)

type InLineError = (Maybe Column, String)

type Parser a = ErrorT InLineError (State (Int, [T.Text])) a

instance Error InLineError where
  strMsg s = (Nothing, s)

parseTable :: Parser a -> T.Text -> Either ParserError [a]
parseTable parser text = parseRows parser (T.lines text) 
                         
parseRows :: Parser a -> [T.Text] -> Either ParserError [a]
parseRows parser rows = forM (zip [1..] rows) $ \(r, line) -> do
  case evalState (runErrorT parser) (0, T.words line) of
    Left  e -> Left $ ParserError { row = r, col = fst e, message = snd e }
    Right v -> Right v

throwE :: String -> Parser a
throwE s = do
  (i, _) <- get
  throwError (Just i, s)

feed :: Parser T.Text
feed = do
  (i, xs) <- get
  guard (not . null $ xs)
  put (i+1, tail xs)
  return $ head xs

liftReader :: String -> R.Reader a -> Parser a
liftReader name reader = do
  x <- feed `catchError` \_ -> throwError $ strMsg "Requires more columns that exists"
  case reader x of
    Left e -> throwE $ e ++ ". " ++ name ++ " parser has been used here"
    Right (v,r) -> do
      guard (T.null r) 
        `catchError` \_ -> throwE $ "parser " ++ name ++ " meet unexpected symbol"
      return v

double :: Parser Double
double = liftReader "double" R.double

float :: Parser Float
float = double2Float <$> double

decimal :: Parser Integer
decimal = liftReader "decimal" R.decimal

integral :: Parser Integer
integral = liftReader "integral" (R.signed R.decimal)

string :: ErrorT InLineError (State (Int, [T.Text])) String
string = T.unpack <$> feed

skip :: ErrorT InLineError (State (Int, [T.Text])) ()
skip = feed >> return ()
