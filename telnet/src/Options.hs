{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Options where

import Data.Word (Word8)

newtype Option = Option Word8 deriving (Eq, Show)

instance Bounded Option where
  minBound = Option 0
  maxBound = Option 255

pattern BinaryTransmission :: Option
pattern BinaryTransmission = Option 0

pattern Echo :: Option
pattern Echo = Option 1

pattern ReconnectionProtocol :: Option
pattern ReconnectionProtocol = Option 2

pattern SuppressGoAhead :: Option
pattern SuppressGoAhead = Option 3

pattern NegotiateApproxMsgSize :: Option
pattern NegotiateApproxMsgSize = Option 4

pattern Status :: Option
pattern Status = Option 5

pattern TimingMark :: Option
pattern TimingMark = Option 6

pattern RemoteControlledTransmissionAndEcho :: Option
pattern RemoteControlledTransmissionAndEcho = Option 7

pattern NegotiateAboutOutputLineWidth :: Option
pattern NegotiateAboutOutputLineWidth = Option 8

pattern NegotiateAboutOutputPageSize :: Option
pattern NegotiateAboutOutputPageSize = Option 9

pattern NegotiateAboutOutputCrDisposition :: Option
pattern NegotiateAboutOutputCrDisposition = Option 10

pattern NegotiateAboutOutputHorizontalTabStops :: Option
pattern NegotiateAboutOutputHorizontalTabStops = Option 11

pattern NegotiateAboutOutputHorizontalTabDisposition :: Option
pattern NegotiateAboutOutputHorizontalTabDisposition = Option 12

pattern NegotiateAboutOutputFormFeedDisposition :: Option
pattern NegotiateAboutOutputFormFeedDisposition = Option 13

pattern NegotiateAboutOutputVerticalTabStops :: Option
pattern NegotiateAboutOutputVerticalTabStops = Option 14

pattern NegotiateAboutOutputVerticalTabDisposition :: Option
pattern NegotiateAboutOutputVerticalTabDisposition = Option 15

pattern NegotiateAboutOutputLineFeedDisposition :: Option
pattern NegotiateAboutOutputLineFeedDisposition = Option 16

pattern ExtendedAscii :: Option
pattern ExtendedAscii = Option 17

pattern Logout :: Option
pattern Logout = Option 18

pattern ByteMacro :: Option
pattern ByteMacro = Option 19

pattern DataEntryTerminal :: Option
pattern DataEntryTerminal = Option 20

pattern Supdup :: Option
pattern Supdup = Option 21

pattern SupdupOutput :: Option
pattern SupdupOutput = Option 22

pattern SendLocation :: Option
pattern SendLocation = Option 23

pattern TerminalType :: Option
pattern TerminalType = Option 24

pattern EndOfRecord :: Option
pattern EndOfRecord = Option 25

pattern TacacsUserIdentification :: Option
pattern TacacsUserIdentification = Option 26

pattern OutputMarking :: Option
pattern OutputMarking = Option 27

pattern TerminalLocationNumber :: Option
pattern TerminalLocationNumber = Option 28

pattern Ibm3270Regime :: Option
pattern Ibm3270Regime = Option 29

pattern X3Pad :: Option
pattern X3Pad = Option 30

pattern NegotiateAboutWindowSize :: Option
pattern NegotiateAboutWindowSize = Option 31

pattern TerminalSpeed :: Option
pattern TerminalSpeed = Option 32

pattern RemoteFlowControl :: Option
pattern RemoteFlowControl = Option 33

pattern Linemode :: Option
pattern Linemode = Option 34

pattern XDisplayLocation :: Option
pattern XDisplayLocation = Option 35

pattern Environment :: Option
pattern Environment = Option 36

pattern Authentication :: Option
pattern Authentication = Option 37

pattern Encryption :: Option
pattern Encryption = Option 38

pattern NewEnvironment :: Option
pattern NewEnvironment = Option 39

pattern Tn3270Enhancements :: Option
pattern Tn3270Enhancements = Option 40

pattern XAuth :: Option
pattern XAuth = Option 41

pattern CharacterSet :: Option
pattern CharacterSet = Option 42

pattern RemoteSerialPort :: Option
pattern RemoteSerialPort = Option 43

pattern ComPort :: Option
pattern ComPort = Option 44

pattern SupressLocalEcho :: Option
pattern SupressLocalEcho = Option 45

pattern StartTls :: Option
pattern StartTls = Option 46

pattern Kermit :: Option
pattern Kermit = Option 47

pattern MudServerStatusProtocol :: Option
pattern MudServerStatusProtocol = Option 70

pattern Compression1 :: Option
pattern Compression1 = Option 85

pattern MudClientCompressionProtocol :: Option
pattern MudClientCompressionProtocol = Option 85

pattern Compression2 :: Option
pattern Compression2 = Option 86

pattern MudClientCompressionProtocolV2 :: Option
pattern MudClientCompressionProtocolV2 = Option 86

pattern Compression3 :: Option
pattern Compression3 = Option 87

pattern MudClientCompressionProtocolV3 :: Option
pattern MudClientCompressionProtocolV3 = Option 87

pattern ZenithMudProtocol :: Option
pattern ZenithMudProtocol = Option 93

pattern ExtendedOptionsList :: Option
pattern ExtendedOptionsList = Option 255
