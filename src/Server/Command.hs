{-# OPTIONS -Wall  -fno-warn-unused-do-bind #-}
{-# LANGUAGE ExistentialQuantification #-}


{- |
Module      :  Server.Command
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Server.Command
 ( Obj(..)
 , unknown
 , echo
 , task
 )
   where

import Control.Monad.Reader       ( ReaderT, MonadIO, asks, liftIO )
import Data.Object                ( Object(..) )
import Server.Context             ( Context(..) )
import Server.Response
import System.IO                  ( Handle )
import Text.Printf                ( printf )

import qualified Server.Command.ANN as ANN

data Obj = forall a b . (ANN.Obj a, Object a b) => Obj (a,b)

--
unknown :: Handle -> String -> ReaderT Context IO ()
unknown handle _ = sendRespErr_ handle $ GenErrResp "Unknown command"

--
echo :: Handle -> String -> ReaderT Context IO ()
echo handle c = sendRespOk_ handle $ GenOkResp c


--
task :: Handle -> Obj -> ReaderT Context IO ()
task handle (Obj obj) = do
  --
  doEstimate >>= doResponse

    where
      --
      doEstimate :: ReaderT Context IO (Maybe [(Int, Float)])
      doEstimate = do

        liftIO $ printf "doEstimate : %s\n" (show obj)

        sendRespOk handle $ TaskInfoResp "doing estimation"

        -- Запрашиваем тип распределения для генерации выборки объектов.
        objDist <- asks $ rndObjDist

        -- Запрашиваем количество объектов в генерируемой выборке.
        numObjs <- asks $ numRndObjs

        -- Генерация выборки заданного объема на основе оценки параметров
        -- объекта и их точности.
        objs <- liftIO $ genRandomObjects objDist numObjs obj

        -- 
        ANN.estimate obj objs

      --
      doResponse :: Maybe [(Int, Float)] -> ReaderT Context IO ()
      doResponse pd = do 

        let msg = if pd == Nothing 
                     then "Response is null because kdtree was not found."
                     else "Server is sending the response."

        sendRespOk handle $ TaskInfoResp msg 

        sendRespOk_ handle $ TaskResultResp pd
