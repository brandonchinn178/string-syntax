module Lib where

import SqlQuery

sqlVal :: ToSqlValue a => a -> SqlQuery
sqlVal a = SqlQuery "?" [toSqlValue a]
