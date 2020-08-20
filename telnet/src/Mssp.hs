{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Mssp where

import Data.Word (Word8)

newtype Type = Type Word8 deriving (Eq, Show)

instance Bounded Type where
  minBound = Type 1
  maxBound = Type 2

pattern Variable :: Type
pattern Variable = Type 1

pattern Value :: Type
pattern Value = Type 2
