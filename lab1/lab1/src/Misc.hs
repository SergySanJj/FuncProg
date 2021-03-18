{-# LANGUAGE FlexibleInstances #-}
module Misc where

import qualified System.IO.Streams as Streams
import Database.MySQL.Base ( MySQLValue(MySQLText, MySQLInt64U) )
import qualified Data.ByteString.Char8    as BS
import qualified Data.Text

pairToList :: (a, a) -> [a]
pairToList (x,y) = [x,y]

tuplesToList :: [(a,a)] -> [[a]]
tuplesToList = map pairToList


-- MySql query types converters --
class MySqlConverter a where
    toMySql :: a -> MySQLValue 

instance MySqlConverter String where
    toMySql s = MySQLText $ Data.Text.pack s

instance MySqlConverter Integer  where
    toMySql i = MySQLInt64U $ fromIntegral i


toMySqlParams :: (MySqlConverter p) => [p] -> [MySQLValue]
toMySqlParams list =  [toMySql (last list)]


readInt :: String -> Integer 
readInt i = read  i :: Integer

