{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module QState where

import qualified Commands as Cmds
import           Data.Bits
  ( Bits,
    (.&.),
    (.|.),
  )
import           Data.Int (Int8)
import qualified Options as Opts

newtype State = State Int8 deriving (Eq, Show)
instance Bounded State where

  minBound = State (-128)
  maxBound = State 127

pattern NoBit :: Int8
pattern NoBit = 0x0
pattern YesBit :: Int8

pattern YesBit = 0x1

pattern WantBit :: Int8
pattern WantBit = 0x2

pattern OppositeBit :: Int8
pattern OppositeBit = 0x4

pattern No :: State
pattern No = State NoBit

pattern Yes :: State
pattern Yes = State YesBit

pattern WantNo :: State
pattern WantNo <-
  State ((== WantBit .&. NoBit) -> True)
  where
    WantNo = State (WantBit .&. NoBit)

pattern WantYes :: State
pattern WantYes <-
  State ((== WantBit .&. YesBit) -> True)
  where
    WantYes = State (WantBit .&. YesBit)

pattern WantNoOpposite :: State
pattern WantNoOpposite <-
  State ((== WantBit .&. NoBit .&. OppositeBit) -> True)
  where
    WantNoOpposite = State (WantBit .&. NoBit .&. OppositeBit)

pattern WantYesOpposite :: State
pattern WantYesOpposite <-
  State ((== WantBit .&. YesBit .&. OppositeBit) -> True)
  where
    WantYesOpposite = State (WantBit .&. YesBit .&. OppositeBit)

create :: State -> State -> State
create (State local) (State remote) = State $ local .|. remote

localState :: State -> State
localState (State state) = State $ state .&. 15

remoteState :: State -> State
remoteState (State state) = State $ state .&. (-16)
