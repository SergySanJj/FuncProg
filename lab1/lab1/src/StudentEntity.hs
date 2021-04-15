{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TemplateHaskell #-}

module StudentEntity where

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

unpack [MySQLInt64U id, MySQLText name] = (fromIntegral id, Data.Text.unpack name)

data StudentData = StudentData {uid :: Integer, name :: String} deriving (Show)
setName :: StudentData -> String -> StudentData
setName student = StudentData (uid student)

instance ToJSON StudentData where
 toJSON (StudentData uid name) =
    object [ "uid"  .= uid
           , "name"   .= name
             ]

instance Entity StudentData where
    getEntity conn studentId = do
        s <- prepareStmt conn "SELECT * FROM `students` where students.uid_ = ?"
        (defs, is) <- queryStmt conn s $ toMySqlParams [studentId]

        (rows ::[[MySQLValue]]) <- Streams.toList is
        let selected = last (map unpack rows)

        return $ StudentData (sel1 selected) (sel2 selected)

    updateEntity conn student = execute conn q [toMySql $ name student, toMySql $ uid student]
        where
            q = "UPDATE `students` SET name = ? where students.uid_ = ?;"

    createEntity conn student = do
        print "here"
        execute conn q [toMySql $ name student]
        where
            q = "insert into students (name) values (?)"

    getAll conn = do
        (defs, is) <- query_ conn "Select * from students"
        (rows ::[[MySQLValue]]) <- Streams.toList is
        return $ toStudent rows


toStudent :: [[MySQLValue]] -> [StudentData]
toStudent rows = do
    let unpacked = map unpack rows
    map (\x -> StudentData (sel1 x) (sel2 x)) unpacked