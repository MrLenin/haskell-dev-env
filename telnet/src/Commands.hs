{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Commands where

import Data.Word (Word8)

newtype Command
  = Command Word8
  deriving (Eq, Show)

instance Bounded Command where
  minBound = Command 236
  maxBound = Command 255

pattern InterpretAsCommand :: Command
pattern InterpretAsCommand = Command 255

pattern Dont :: Command
pattern Dont = Command 254

pattern Do :: Command
pattern Do = Command 253

pattern Wont :: Command
pattern Wont = Command 252

pattern Will :: Command
pattern Will = Command 251

pattern SubnegotiationBegin :: Command
pattern SubnegotiationBegin = Command 250

pattern GoAhead :: Command
pattern GoAhead = Command 249

pattern EraseLine :: Command
pattern EraseLine = Command 248

pattern EraseCharacter :: Command
pattern EraseCharacter = Command 247

pattern AreYouThere :: Command
pattern AreYouThere = Command 246

pattern AbortOutput :: Command
pattern AbortOutput = Command 245

pattern InterruptProcess :: Command
pattern InterruptProcess = Command 244

pattern Break :: Command
pattern Break = Command 243

pattern DataMark :: Command
pattern DataMark = Command 242

pattern NoOperation :: Command
pattern NoOperation = Command 241

pattern SubnegotiationEnd :: Command
pattern SubnegotiationEnd = Command 240

pattern EndOfRecord :: Command
pattern EndOfRecord = Command 239

pattern AbortExecution :: Command
pattern AbortExecution = Command 238

pattern SuspendExecution :: Command
pattern SuspendExecution = Command 237

pattern EndOfFile :: Command
pattern EndOfFile = Command 236
