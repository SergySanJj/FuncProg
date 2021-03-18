{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import Misc
import Entity
import StudentEntity

import Web.Scotty
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.List
import Data.Text.Lazy
import qualified Data.Maybe
import Data.Aeson


main :: IO ()
main = do
   conn <- connect
      defaultConnectInfo {ciUser = "root", ciPassword = "13376969", ciDatabase = "lab1"}
   
   
   scotty 3000 $ do
      get "/students/" $ do
         students <- liftAndCatchIO (Entity.getAll conn :: IO [StudentData])
         Web.Scotty.text $ repr students

      get "/student/" $ do
         (uid :: Integer) <- param "uid" 
         st <- liftAndCatchIO (Entity.getEntity conn uid :: IO StudentData)
         Web.Scotty.json $ toJSON st

      post "/student/" $ do
         (name_ :: String) <- param "name" 
         res <- liftAndCatchIO (Entity.createEntity conn (StudentData 0 name_) :: IO OK)
         Web.Scotty.text $ pack(show res)

studentInfo :: MySQLConn -> IO Text
studentInfo conn = do
   let rr = Entity.getAll conn :: IO [StudentData]
   p <- rr
   let ppp = mconcat (Data.List.map (pack . show) p)
   return ppp

repr :: [StudentData] -> Text
repr st = "[" <> mconcat (Data.List.intersperse ", " (Data.List.map (pack . show) st)) <> "]"