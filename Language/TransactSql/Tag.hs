{-# LANGUAGE FlexibleInstances #-}

module Language.TransactSql.Tag
       ( Tag (..)
       ) where

class Tag t where
  untag :: t a -> a
