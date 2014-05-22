{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
Module      :  Server.Response
Description :  <optional short text displayed on contents page>
Copyright   :  (c) Sergey Sichevskiy 2013
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable

-}

module Server.Response where

import Control.Applicative  ( (<$>), (<*>) )
import Control.Monad.Error  ( Error(..) )
import Control.Monad.Reader ( MonadReader, MonadIO, liftIO, asks )
import Data.Word            ( Word64 )
import Data.Serialize       ( Serialize(..), encode, putWord8, getWord8 )
--import Data.Binary          ( Binary(..), encode, putWord8, getWord8 )
import Server.Context       ( Context, serverName )
import System.IO            ( Handle )

import GHC.Generics (Generic)

import qualified Data.ByteString.Char8 as B

type Name = String
type TaskNumber = String

data Response 
        = OkResp Name OkContent
        | ErrResp Name ErrContent
        deriving (Generic, Show, Read)

data OkContent
        = GenOkResp String
        | TaskInfoResp String
        | TaskResultResp (Maybe [(Int,Float)])
        deriving (Show, Read, Generic)

data ErrContent
        = GenErrResp String
        | TaskErrResp String
        deriving (Show, Read, Generic)

instance Error ErrContent where
  noMsg    = GenErrResp "Occurred a server error!"
  strMsg s = GenErrResp s


-- |
sendRespOk ::  (MonadReader (Context) m, MonadIO m) => Handle -> OkContent -> m ()
sendRespOk handle cont = do

   name <- asks serverName

   let resp = OkResp name cont
       cmd  = encode resp
       num  = encode (fromIntegral (B.length cmd) :: Word64)

   liftIO $ do 
     B.hPutStr handle num
     B.hPutStr handle cmd

     putStrLn (show resp)

sendRespOk_ ::  (MonadReader (Context) m, MonadIO m) => Handle -> OkContent -> m ()
sendRespOk_ handle cont = do 

   name <- asks serverName

   let resp = OkResp name cont
       cmd  = encode resp
       num  = encode (fromIntegral (B.length cmd) :: Word64)

   liftIO $ do 
     B.hPutStr handle num
     B.hPutStr handle cmd

-- |
sendRespErr ::  (MonadReader (Context) m, MonadIO m) => Handle -> ErrContent -> m ()
sendRespErr handle cont = do

   name <- asks serverName

   let resp = ErrResp name cont
       cmd  = encode resp
       num  = encode (fromIntegral (B.length cmd) :: Word64)

   liftIO $ do 
     B.hPutStr handle num
     B.hPutStr handle cmd

     putStrLn (show resp)

sendRespErr_ ::  (MonadReader (Context) m, MonadIO m) => Handle -> ErrContent -> m ()
sendRespErr_ handle cont = do

  name <- asks serverName

  let resp = ErrResp name cont
      cmd  = encode resp
      num  = encode (fromIntegral (B.length cmd) :: Word64)

  liftIO $ do 
     B.hPutStr handle num
     B.hPutStr handle cmd

{--
instance Binary Response
instance Binary OkContent
instance Binary ErrContent
--}


--{--
instance Serialize Response where
  put ( OkResp n c) = putWord8 0 >> put n >> put c
  put (ErrResp n c) = putWord8 1 >> put n >> put c

  get = do
    tag <- getWord8
    case tag of
      0 ->  OkResp <$> get <*> get
      _ -> ErrResp <$> get <*> get

instance Serialize OkContent where
  put (GenOkResp s) = putWord8 0 >> put s
  put (TaskInfoResp s) = putWord8 1 >> put s
  put (TaskResultResp r) = putWord8 2 >> put r

  get = do
    tag <- getWord8
    case tag of
      0 ->      GenOkResp <$> get
      1 ->   TaskInfoResp <$> get
      _ -> TaskResultResp <$> get

instance Serialize ErrContent where
  put ( GenErrResp s) = putWord8 0 >> put s
  put (TaskErrResp s) = putWord8 1 >> put s

  get = do
    tag <- getWord8
    case tag of
      0 ->  GenErrResp <$> get
      _ -> TaskErrResp <$> get
--}