{-# LANGUAGE DeriveDataTypeable #-}

module Language.TransactSql.Loc
       ( SrcLoc, noLoc, mkLoc
       , SrcSpan, noSpan, mkSpan, startOf, endOf
       , combineSpan
       , Located (..)
       , GenLocated (..)
       ) where

import Data.Generics (Data, Typeable)
import Text.Parsec.Pos (SourcePos)

data SrcLoc
  = SrcLoc SourcePos
  | BadLoc String
  deriving (Eq, Ord, Data, Typeable)

data SrcSpan
  = SrcSpan SourcePos SourcePos
  | BadSpan String
  deriving (Eq, Ord, Data, Typeable)
           
instance Show SrcLoc where
  showsPrec d (SrcLoc p) = showsPrec d p
  showsPrec d (BadLoc str) = showString str

instance Show SrcSpan where
  showsPrec d (SrcSpan start end)
    | start == end = showsPrec d start
    | otherwise = showsPrec d start . showString "-" . showsPrec d end
  showsPrec d (BadSpan str) = showString str

mkLoc = SrcLoc
noLoc = BadLoc "Unknown location"
mkSpan = SrcSpan
noSpan = BadSpan "Unknown location"
startOf (SrcSpan s _) = SrcLoc s
startOf (BadSpan m) = BadLoc m
endOf (SrcSpan _ e) = SrcLoc e
endOf (BadSpan m) = BadLoc m
combineSpan (SrcSpan s _) (SrcSpan _ e) = SrcSpan s e
combineSpan s@(SrcSpan _ _) _ = s
combineSpan (BadSpan m) (BadSpan _) = BadSpan m
combineSpan (BadSpan _) s = s

type Located a = GenLocated SrcSpan a

data GenLocated l a
    = L { getLoc :: l, unLoc :: a }
    deriving (Eq, Ord, Data, Typeable)

instance Show a => Show (GenLocated l a) where
  showsPrec d (L _ x) = showsPrec d x
  show (L _ x) = show x

instance Functor (GenLocated l) where
  fmap f (L l x) = L l (f x)
