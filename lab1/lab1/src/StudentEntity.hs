{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module StudentEntity where

import Misc
import Entity

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Char8    as BS
import qualified Data.Text
import           Data.List
import           Data.Tuple.Select

unpack [MySQLInt64U id, MySQLText name] = (fromIntegral id, Data.Text.unpack name)

data StudentData = StudentData {id :: Integer, name :: String} deriving (Show)
getId :: StudentData -> Integer
getId (StudentData x _) = x
getName :: StudentData -> String 
getName (StudentData _ x) = x 

setName :: StudentData -> String -> StudentData
setName student = StudentData (getId student)



instance Entity StudentData where
    getEntity conn studentId = do
        s <- prepareStmt conn "SELECT * FROM `students` where students.id = ?"
        (defs, is) <- queryStmt conn s $ toMySqlParams [studentId]

        (rows ::[[MySQLValue]]) <- Streams.toList is
        let selected = last (map unpack rows)

        return $ StudentData (sel1 selected) (sel2 selected)

    updateEntity conn student = do
        execute conn q [toMySql $ getName student, toMySql $ getId student]
        where
            q = "UPDATE `students` SET name = ? where students.id = ?;"
    
    createEntity conn student = do
        execute conn q [toMySql $ getName student]
        where
            q = "insert into lab1_student (name) values (?)"

