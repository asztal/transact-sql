{-# LANGUAGE DeriveDataTypeable #-}

module Language.TransactSql.Types
       ( SqlType(..)
       , Len(..)
       , Precision
       , Scale
       , Collation
       , precision
       , scale
       , len
       ) where

import Data.Generics (Data, Typeable)

type Precision = Int
data Len = Varying Int Int
         | Fixed Int
         | Unlimited
         deriving (Eq, Ord, Show, Read, Data, Typeable)
type Scale = Int
type Collation = String

data SqlType
  = Bit
  | Numeric Precision Scale | Decimal Precision Scale
  | Money | SmallMoney
  | BigInt | Int | SmallInt | TinyInt
  | Float Int
  | Date | Time Scale | DateTime | DateTime2 Scale | SmallDateTime | DateTimeOffset Scale
  | Char Int | VarChar Int | Text
  | NChar Int | NVarChar Int | NText
  | Binary Int | VarBinary Int | Image
  | Cursor
  | TimeStamp
  | HierarchyId
  | UniqueIdentifier
  | SqlVariant
  | Xml
  | Table
  | Geometry
  | Geography
  deriving (Eq, Ord, Show, Read, Data, Typeable)

precisionFromTimeScale 0 = 0
precisionFromTimeScale p = p + 1

precision :: SqlType -> Maybe Precision
precision Bit = Just 1
precision (Numeric p _) = Just p
precision (Decimal p _) = Just p
precision (Float bits)
  | bits <= 24 = Just 7
  | otherwise = Just 15
precision Money = Just 19
precision SmallMoney = Just 10
precision Date = Just 10
precision DateTime = Just 23
precision SmallDateTime = Just 16
precision (DateTimeOffset s) = Just (26 + precisionFromTimeScale s)
precision (DateTime2 s) = Just (19 + precisionFromTimeScale s)
precision (Time s) = Just (8 + precisionFromTimeScale s)
precision _ = Nothing

scale :: SqlType -> Maybe Scale
scale BigInt = Just 0
scale Int = Just 0
scale SmallInt = Just 0
scale TinyInt = Just 0
scale Money = Just 4
scale SmallMoney = Just 4
scale Date = Just 0
scale DateTime = Just 3
scale SmallDateTime = Just 0
scale (DateTimeOffset s) = Just s
scale (DateTime2 s) = Just s
scale (Time s) = Just s
scale _ = Nothing

-- Valid for numeric(p,s) and decimal(p,s)
lengthFromPrecision :: Precision -> Int
lengthFromPrecision p
  | p <= 9 = 5
  | p <= 19 = 9
  | p <= 28 = 13
  | otherwise = 17

lengthFromTimeScale :: Scale -> Int
lengthFromTimeScale s
  | s <= 2 = 0
  | s <= 4 = 1
  | otherwise = 2

len :: SqlType -> Len
len Bit = Fixed 1
len (Numeric p _) = Fixed (lengthFromPrecision p)
len (Decimal p _) = Fixed (lengthFromPrecision p)
len (Char n) = Fixed n
len (VarChar (-1)) = Unlimited
len (VarChar n) = Varying 0 n
len (NChar n) = Fixed (n * 2)
len (NVarChar (-1)) = Unlimited
len (NVarChar n) = Varying 0 (n * 2)
len (Binary n) = Fixed n
len (VarBinary (-1)) = Unlimited
len (VarBinary n) = Varying 0 n
len (Float bits)
  | bits <= 24 = Fixed 4
  | otherwise = Fixed 8
len BigInt = Fixed 8
len Int = Fixed 4
len SmallInt = Fixed 2
len TinyInt = Fixed 1
len Date = Fixed 3
len DateTime = Fixed 8
len SmallDateTime = Fixed 4
len (DateTimeOffset s) = Fixed (8 + lengthFromTimeScale s)
len (DateTime2 s) = Fixed (6 + lengthFromTimeScale s)
len (Time s) = Fixed (3 + lengthFromTimeScale s)
len Cursor = Fixed 0
len TimeStamp = Fixed 8
len HierarchyId = Varying 1 892
len UniqueIdentifier = Fixed 16
len SqlVariant = Varying 16 8016
len Xml = Unlimited
len Table = Unlimited
len Geometry = Unlimited
len Geography = Unlimited
len _ = Unlimited
