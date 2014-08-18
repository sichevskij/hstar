{-# OPTIONS -Wall -Werror -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

{- |
Module      :  Server.Server
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Server.Server
  ( reqHandler
  )
    where

import Control.Monad.Reader ( ReaderT, liftIO )
import Server.Context       ( Context )
import Server.Command       ( Obj(..) )
import Server.Request       ( Request(..), Cmd(..) )
import System.IO            ( hGetLine, hIsEOF, Handle )

import qualified Server.Command as Command
import qualified Server.Request as ReqObj

-- |
reqHandler :: Handle -> ReaderT Context IO ()
reqHandler hIn = do

   eof <- liftIO $ hIsEOF hIn

   if not eof
      then do
             cmd <- liftIO $ hGetLine hIn
             exeCmd cmd
             reqHandler hIn
      else do
             return ()
   where
     --
     exeCmd cmd =  do
       -- В зависимости от команды выполняются соответствующие действия.
       case reads cmd of
         (Req (Echo c), _):_  -> Command.echo hIn c
         (Req (Task t), _):_  -> Command.task hIn (toObj t)
         _                    -> Command.unknown hIn cmd
     -- 
     toObj t =
       case t of
         ReqObj.OxGALEX o e -> Obj (o,e)
         ReqObj.OxSDSS o e -> Obj (o,e)
         ReqObj.OxTWOMASS o e -> Obj (o,e)
         ReqObj.OxIPHAS o e -> Obj (o,e)
         ReqObj.OxGRIxTWOMASS o e -> Obj (o,e)
         ReqObj.OxGRIZxTWOMASS o e -> Obj (o,e)
         ReqObj.OxIPHASxTWOMASS o e -> Obj (o,e)
         ReqObj.OxSDSSxTWOMASS o e -> Obj (o,e)
         ReqObj.OxNUVxSDSSxTWOMASS o e -> Obj (o,e)

