{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import Misc
import Entity
import StudentEntity
import CabinetEntity
import WorkplaceEntity

import Web.Scotty
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.List
import Data.Text.Lazy
import qualified Data.Maybe
import Data.Aeson
import Control.Monad.IO.Class

-- main :: IO ()
-- main = do
--       conn <- connect
--           defaultConnectInfo {ciUser = "root", ciPassword = "13376969", ciDatabase = "lab1"}
--       students <- Entity.getAll conn :: IO [StudentData]
--       let s = students :: [StudentData]
--       print s


main :: IO ()
main = do
   conn <- connect
      defaultConnectInfo {ciUser = "root", ciPassword = "13376969", ciDatabase = "lab1"}

   -- ss <- (Entity.getEntity conn 1 :: IO StudentData)
   -- let s = ss :: StudentData
   -- print s

   scotty 3000 $ do
      -- student --
      get "/students/" $ do
         students <- liftAndCatchIO (Entity.getAll conn :: IO [StudentData])
         let s = students :: [StudentData]
         Web.Scotty.json s 

      get "/student/:uid" $ do
         (uid :: Integer) <- param "uid"
         st <- liftAndCatchIO (Entity.getEntity conn uid :: IO StudentData)
         Web.Scotty.json $ toJSON st

      post "/student/:name" $ do
         (name_ :: String) <- param "name"
         res <- liftAndCatchIO (Entity.createEntity conn (StudentData 0 name_) :: IO OK)
         Web.Scotty.text $ pack(show res)

      patch "/student/:uid/:name" $ do
         (uid :: Integer) <- param "uid"
         (st_name :: String) <- param "name"
         res <- liftAndCatchIO (Entity.updateEntity conn (StudentData uid st_name) :: IO OK)
         Web.Scotty.text $ pack(show res)

      -- cabinet -- 
      get "/cabinets/" $ do
         cabinets <- liftAndCatchIO (Entity.getAll conn :: IO [CabinetData])
         let s = cabinets :: [CabinetData]
         Web.Scotty.json s 

      get "/cabinet/:cabinetNumber" $ do
         (cabinetNum :: Integer) <- param "cabinetNumber"
         st <- liftAndCatchIO (Entity.getEntity conn cabinetNum :: IO CabinetData)
         Web.Scotty.json $ toJSON st

      post "/cabinet/:cabinetNum/:workBegin/:workEnd" $ do
         (cabinetNum :: Integer) <- param "cabinetNum"
         (workBegin :: Integer) <- param "workBegin"
         (workEnd :: Integer) <- param "workEnd"
         res <- liftAndCatchIO (Entity.createEntity conn (CabinetData cabinetNum workBegin workEnd) :: IO OK)
         Web.Scotty.text $ pack(show res)

      patch "/cabinet/:cabinetNum/:workBegin/:workEnd" $ do
         (cabinetNum :: Integer) <- param "cabinetNum"
         (workBegin :: Integer) <- param "workBegin"
         (workEnd :: Integer) <- param "workEnd"
         res <- liftAndCatchIO (Entity.updateEntity conn (CabinetData cabinetNum workBegin workEnd) :: IO OK)
         Web.Scotty.text $ pack(show res)

      -- workplace --
      get "/workplaces/" $ do
         workplaces <- liftAndCatchIO (Entity.getAll conn :: IO [WorkplaceData])
         let s = workplaces :: [WorkplaceData]
         Web.Scotty.json s 

      get "/workplace/:uid" $ do
         (uid :: Integer) <- param "uid"
         st <- liftAndCatchIO (Entity.getEntity conn uid :: IO WorkplaceData)
         Web.Scotty.json $ toJSON st

      post "/workplace/:cabinetNum/:studentUid" $ do
         (cabinetNum :: Integer) <- param "cabinetNum"
         (studentUid :: Integer) <- param "studentUid"
         res <- liftAndCatchIO (Entity.createEntity conn (WorkplaceData 0 cabinetNum studentUid) :: IO OK)
         Web.Scotty.text $ pack(show res)

      patch "/workplace/:uid/:cabinetNum/:studentUid" $ do
         (uid :: Integer) <- param "uid"
         (cabinetNum :: Integer) <- param "cabinetNum"
         (studentUid :: Integer) <- param "studentUid"
         res <- liftAndCatchIO (Entity.updateEntity conn (WorkplaceData uid cabinetNum studentUid) :: IO OK)
         Web.Scotty.text $ pack(show res)

studentInfo :: MySQLConn -> IO Text
studentInfo conn = do
   let rr = Entity.getAll conn :: IO [StudentData]
   p <- rr
   let ppp = mconcat (Data.List.map (pack . show) p)
   return ppp
