{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams

import Entity
import StudentEntity

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
