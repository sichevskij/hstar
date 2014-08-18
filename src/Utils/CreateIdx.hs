{-# OPTIONS -Wall -Werror -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      :  Main
Description :  Программа для создания поисковых индексов
Copyright   :  (c) Sergey Sichevskiy 2014
License     :  BSD3

Maintainer  :  s.sichevskij@gmail.com
Stability   :  experimental
Portability :  portable


В данном случае поисковый индекс - это многомерное kd-дерево.

-}

module Main where

import Control.Applicative  ( (<$>) )
import Control.Monad        ( forM_ )
--import Data.Maybe           ( fromJust )
import Data.Point           ( Point13d )
import Data.Time.Clock      ( getCurrentTime, utctDayTime )
import Foreign
import Foreign.C.String     ( CString, newCString )
import Server.Command.ANN   ( Obj( toList) )
import System.Environment   ( getArgs, getProgName )
import System.Exit          ( exitWith, ExitCode(..) )
--import System.IO
--import Text.Printf          ( hPrintf )

--import qualified Data.ByteString.Lazy.Char8 as LB
--import qualified Data.ByteString.Lex.Lazy.Double as LD

import qualified Data.Object as O
import qualified Math.Proj as Proj


type Number  = Int
type TreeDim = Int

data Flag = GALEX | SDSS | IPHAS | TWOMASS | SDSSxTWOMASS | IPHASxTWOMASS | GRIxTWOMASS | GRIZxTWOMASS | NUVxSDSSxTWOMASS | DIMxIPHASxTWOMASS |DIMxNUVxSDSSxTWOMASS deriving (Read, Show)

-- import the foreign function as normal
foreign import ccall "create_kdtree"
  createidx :: CString -> TreeDim -> Number -> (Ptr Float) -> IO ()

foreign import ccall "read_points"
  readdb :: Ptr Point13d -> CString -> IO Int

main :: IO ()
main = do

   (fn, fs) <- getArgs >>= parse

   db <- load fn

   let num = length db

   print $ show (db!!0)

   forM_ fs $ \flg -> do

     let (dim,out,proj) = case flg of
           GALEX   -> (3, "galex.idx", toList . (Proj.proj :: Point13d -> O.GALEX))
           SDSS    -> (5, "sdss.idx",  toList . (Proj.proj :: Point13d -> O.SDSS))
           IPHAS   -> (3, "iphas.idx", toList . (Proj.proj :: Point13d -> O.IPHAS))
           TWOMASS -> (3, "2mass.idx", toList . (Proj.proj :: Point13d -> O.TWOMASS))
           GRIxTWOMASS -> (6, "gri_2mass.idx", toList . (Proj.proj :: Point13d -> O.GRIxTWOMASS))
           GRIZxTWOMASS -> (7, "griz_2mass.idx", toList . (Proj.proj :: Point13d -> O.GRIZxTWOMASS))
           SDSSxTWOMASS -> (8, "sdss_2mass.idx", toList . (Proj.proj :: Point13d -> O.SDSSxTWOMASS))
           IPHASxTWOMASS -> (6, "iphas_2mass.idx", toList . (Proj.proj :: Point13d -> O.IPHASxTWOMASS))
           NUVxSDSSxTWOMASS -> (9, "nuv_sdss_2mass.idx", toList . (Proj.proj :: Point13d -> O.NUVxSDSSxTWOMASS))
           DIMxIPHASxTWOMASS -> (6, "dim_iphas_2mass.idx", toList . (\ (_,_, _,_,_,_,_, j,h,k, r,i,hα) -> O.IPHASxTWOMASS (j,h,k, r,i,hα)))
           DIMxNUVxSDSSxTWOMASS -> (9, "dim_nuv_sdss_2mass.idx", toList . (\ (_,nuv, u,g,r,i,z, j,h,k, _,_,_) -> O.NUVxSDSSxTWOMASS (nuv, u,g,r,i,z, j,h,k)))


     putStrLn ("\nСоздается индекс для " ++ (show flg) ++ ": " ++ out ++ "...")

     ofPtr <- newCString out
     psPtr <- newArray $ concat $ proj <$> db

     createidx ofPtr dim num psPtr

   putStrLn "\nИндексы созданы."

{--
   putStrLn "\nВыполняется создание файла db.txt..."

   hO <- openFile "db.txt" WriteMode

   ps <- return . ((readDouble <$>) . (take 5 . LB.words) <$>) . (drop 1 . LB.lines) =<< LB.readFile fn

   mapM_ (\[a,b,c,d,e] -> hPrintf hO "%f %f %f %f %f\n" a b c d e) ps

   hClose hO
--}

   putStrLn "\nФайл создан. Работа программы закончена."

--   where
--     readDouble = fst . fromJust . LD.readDouble

parse :: [String] -> IO (FilePath, [Flag])
parse argv = case argv of

  fn : s : _ -> case reads s of
                      (fs, _) : _ -> return ( fn, fs )
                      _           -> usage
  _            -> usage

 where
    usage = do
      pn <- getProgName
      putStrLn ("Usage : "++ pn ++" DATA_FILE [GALEX|SDSS|IPHAS|TWOMASS|IPHASxTWOMASS|SDSSxTWOMASS|NUVxSDSSxTWOMASS]") >> exitWith ExitSuccess

load :: FilePath -> IO [Point13d]
load fn = do

   stime <- getCurrentTime >>= return . utctDayTime
   putStrLn "Выполняется загрузка данных из файла..."

   -- В первой строке файла должно быть число равное количеству точек,
   -- представленных в данном файле.
   fl <- return . (head . lines) =<< readFile fn
   num <- case (reads fl) of
            (n, "") : _ -> return n
            _           -> error msg -- putStrLn msg >> exitWith (ExitFailure 0)

   -- Выделяем память для нужного количества элементов.
   dbPtr <- mallocArray num

   -- Считываем данные из файла с возвратом количества прочитанных строк.
   k <- newCString fn >>= readdb dbPtr

   -- Преобразуем в список.
   db <- peekArray k dbPtr

   etime <- getCurrentTime >>= return . utctDayTime
   putStrLn ("Время загрузки данных : " ++ (show $ etime - stime))

   return db
   
   where
     msg = "В первой строке файла '" ++ fn ++ "' должно быть число равное количеству точек, представленных в данном файле."