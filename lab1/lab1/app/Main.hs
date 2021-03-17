{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.List

import Entity
import StudentEntity
import Data.Text.Lazy

main :: IO ()
main = do
   conn <- connect
      defaultConnectInfo {ciUser = "root", ciPassword = "13376969", ciDatabase = "lab1"}

   let res = Entity.getEntity conn 1 :: IO StudentData
   print =<< res
   pp <- res
   print $ name pp
   let changed = setName pp "giovanni d"
   print $ name changed

   let res2 = Entity.updateEntity conn changed :: IO OK
   print =<< res2

   let rr = Entity.getAll conn :: IO [StudentData]
   p <- rr
   let pppp =  mconcat (Data.List.map (pack . show) p)
   
   scotty 3000 $ do
      get "/" $ do
         Web.Scotty.text pppp

studentInfo :: MySQLConn -> IO Text
studentInfo conn = do
   let rr = Entity.getAll conn :: IO [StudentData]
   p <- rr
   let ppp = mconcat (Data.List.map (pack . show) p)
   return ppp