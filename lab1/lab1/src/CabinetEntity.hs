{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module CabinetEntity where

import Misc
import Entity

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Char8    as BS
import qualified Data.Text
import           Data.List
import           Data.Tuple.Select
import Data.Aeson
import Data.Text (Text)

unpack [MySQLInt64U num, MySQLInt64U begin_, MySQLInt64U end_] = (fromIntegral num, fromIntegral begin_, fromIntegral end_)

data CabinetData = CabinetData {cabinetNumber :: Integer, workBegin :: Integer, workEnd :: Integer } deriving (Show)
setBegin :: CabinetData -> Integer -> CabinetData
setBegin obj value = CabinetData (cabinetNumber obj) value (workEnd obj)
setEnd :: CabinetData -> Integer -> CabinetData
setEnd obj = CabinetData (cabinetNumber obj) (workBegin obj)

instance ToJSON CabinetData where
 toJSON (CabinetData cabinetNumber workBegin workEnd) =
    object [ "cabinetNumber"  .= cabinetNumber
           , "workBegin"   .= workBegin
           , "workEnd" .= workEnd
             ]

instance Entity CabinetData where
    getEntity conn cabinetNum = do
        s <- prepareStmt conn "SELECT * FROM `cabinets` where num = ?"
        (defs, is) <- queryStmt conn s $ toMySqlParams [cabinetNum]

        (rows ::[[MySQLValue]]) <- Streams.toList is
        let selected = last (map unpack rows)

        return $ CabinetData (sel1 selected) (sel2 selected) (sel3 selected)

    updateEntity conn cabinet = execute conn q [toMySql $ cabinetNumber cabinet, toMySql $ workEnd cabinet, toMySql $ cabinetNumber cabinet]
        where
            q = "UPDATE `cabinets` SET begin_ = ?, SET end = ? where num = ?;"

    createEntity conn cabinet = execute conn q [toMySql $ workBegin cabinet, toMySql $ workBegin cabinet, toMySql $ workEnd cabinet]
        where
            q = "insert into cabinets (num, begin_, end_) values (?, ?, ?)"

    getAll conn = do
        (defs, is) <- query_ conn "Select * from cabinets"
        (rows ::[[MySQLValue]]) <- Streams.toList is
        return $ toCabinet rows

toCabinet :: [[MySQLValue]] -> [CabinetData]
toCabinet rows = do
    let unpacked = map unpack rows
    map (\x -> CabinetData (sel1 x) (sel2 x) (sel3 x)) unpacked