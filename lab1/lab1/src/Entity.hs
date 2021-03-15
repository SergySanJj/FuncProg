{-# LANGUAGE ScopedTypeVariables #-}

module Entity where

import Misc

import Database.MySQL.Base
import qualified System.IO.Streams as Streams

class Entity a where
    getEntity :: MySQLConn -> Integer -> IO a
    updateEntity :: MySQLConn -> a -> IO OK
    createEntity :: MySQLConn -> a -> IO OK