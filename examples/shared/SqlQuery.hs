module SqlQuery where

import Data.String (IsString (..))

data SqlQuery = SqlQuery
  { sqlText :: String
  , sqlValues :: [SqlValue]
  }
  deriving (Show)

instance IsString SqlQuery where
  fromString s = SqlQuery s []
instance Semigroup SqlQuery where
  q1 <> q2 =
    SqlQuery
      { sqlText = sqlText q1 <> sqlText q2
      , sqlValues = sqlValues q1 <> sqlValues q2
      }
instance Monoid SqlQuery where
  mempty =
    SqlQuery
      { sqlText = ""
      , sqlValues = []
      }

data SqlValue
  = SqlString String
  | SqlInt Int
  deriving (Show)

class ToSqlValue a where
  toSqlValue :: a -> SqlValue
instance ToSqlValue Int where
  toSqlValue = SqlInt
instance ToSqlValue String where
  toSqlValue = SqlString
