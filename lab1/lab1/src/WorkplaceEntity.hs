{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module WorkplaceEntity where

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

unpack [MySQLInt64U uid, MySQLInt32 cabinetNum, MySQLInt32 studentUid] = (fromIntegral uid, fromIntegral cabinetNum, fromIntegral studentUid)

data WorkplaceData = WorkplaceData {uid :: Integer, cabinetNum :: Integer, studentUid :: Integer } deriving (Show)
setCabinetNum :: WorkplaceData -> Integer -> WorkplaceData
setCabinetNum obj value = WorkplaceData (uid obj) value (studentUid obj)
setStudentUid :: WorkplaceData -> Integer -> WorkplaceData
setStudentUid obj = WorkplaceData (uid obj) (cabinetNum obj)

instance ToJSON WorkplaceData where
 toJSON (WorkplaceData uid cabinetNum studentUid) =
    object [ "cabinet_num"  .= cabinetNum
           , "uid_"   .= uid
           , "student_uid_" .= studentUid
             ]

instance Entity WorkplaceData where
    getEntity conn uid = do
        s <- prepareStmt conn "SELECT * FROM `workplaces` where uid_ = ?"
        (defs, is) <- queryStmt conn s $ toMySqlParams [uid]

        (rows ::[[MySQLValue]]) <- Streams.toList is
        let selected = last (map unpack rows)

        return $ WorkplaceData (sel1 selected) (sel2 selected) (sel3 selected)

    updateEntity conn workplace = execute conn q [toMySql $ studentUid workplace, toMySql $ cabinetNum workplace, toMySql $ uid workplace]
        where
            q = "UPDATE `workplaces` SET student_uid_ = ?, SET cabinet_num = ? where uid_ = ?;"

    createEntity conn workplace = execute conn q [toMySql $ cabinetNum workplace, toMySql $ studentUid workplace]
        where
            q = "insert into workplaces (cabinet_num, student_uid_) values (?, ?)"

    getAll conn = do
        (defs, is) <- query_ conn "Select * from workplaces"
        (rows ::[[MySQLValue]]) <- Streams.toList is
        print rows
        return $ toWorkplace rows

toWorkplace :: [[MySQLValue]] -> [WorkplaceData]
toWorkplace rows = do
    let unpacked = map unpack rows
    map (\x -> WorkplaceData (sel1 x) (sel2 x) (sel3 x)) unpacked