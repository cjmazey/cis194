module Monoids where

import           Data.Monoid

newtype Cbool =
  Cbool { unCbool :: Bool }
  deriving (Eq, Show)

newtype Dbool =
  Dbool { unDbool :: Bool }
  deriving (Eq, Show)

instance Monoid Cbool where
  mempty = Cbool True
  mappend (Cbool p) (Cbool q) = Cbool $ p && q

instance Monoid Dbool where
  mempty = Dbool False
  mappend (Dbool p) (Dbool q) = Dbool $ p || q

newtype Function a b =
  Function { unFunction :: a -> b }

instance Monoid b => Monoid (Function a b) where
  mempty = Function $ const mempty
  mappend (Function f) (Function g) =
    Function $ \ x -> f x <> g x
