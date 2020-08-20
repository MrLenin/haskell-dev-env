{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Environ where

import Data.Word (Word8)

newtype Command = Command Word8 deriving (Eq, Show)

instance Bounded Command where
  minBound = Command 0
  maxBound = Command 2

newtype Type = Type Word8 deriving (Eq, Show)
instance Bounded Type where

  minBound = Type 0
  maxBound = Type 3

pattern Is :: Command
pattern Is = Command 0

pattern Send :: Command
pattern Send = Command 1

pattern Info :: Command
pattern Info = Command 2

pattern Var :: Type
pattern Var = Type 0

pattern Val :: Type
pattern Val = Type 1

pattern Esc :: Type
pattern Esc = Type 2

pattern User :: Type
pattern User = Type 3
