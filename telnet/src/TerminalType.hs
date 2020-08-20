{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module TerminalType where

import Data.Word (Word8)

newtype Command = Command Word8 deriving (Eq, Show)

instance Bounded Command where
  minBound = Command 0
  maxBound = Command 1

pattern Is :: Command
pattern Is = Command 0

pattern Send :: Command
pattern Send = Command 1
