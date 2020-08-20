{-# LANGUAGE ViewPatterns #-}

module Lib where

import qualified Commands as Cmds
import           Control.Exception
import           Control.Monad
import           Data.Bits (Bits, bit, zeroBits, (.&.), (.|.))
import qualified Data.ByteString as BS
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Typeable as Typ
import           Data.Word (Word8)
import qualified Environ as Env
import qualified Mssp
import qualified Options as Opts
import qualified QState as Q
import qualified TerminalType as TType

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Flag
  = None
  | Proxy
  | NvtEndOfLine
  | TransmitBinary
  | ReceiveBinary
  | Deflate
  deriving (Eq, Show)

data State
  = ProcessData
  | ProcessEol
  | ProcessIac
  | ProcessWill
  | ProcessWont
  | ProcessDo
  | ProcessDont
  | ProcessSb
  | ProcessSbData
  | ProcessSbDataIac
  | ProcessSkip
  deriving (Eq, Show)

data EnvironmentVariable
  = EnvironmentVariable
      { globalVariable :: T.Text,
        globalValue :: [T.Text]
      }
  | UserEnvironmentVariable
      { userVariable :: T.Text,
        userValue :: [T.Text]
      }

data MsspVariable = MsspVariable
  { msspVariable :: T.Text,
    msspValue :: [T.Text]
  }

data Option = Option
  { option :: Opts.Option,
    local :: Cmds.Command,
    remote :: Cmds.Command
  }

data Negotiation = Negotiation
  { negotiationOption :: Opts.Option,
    negotiationState :: Q.State
  }
  deriving (Eq, Show)

data Event
  = ReceiveData [Word8]
  | SendData [Word8]
  | InterpretAsCommand Cmds.Command
  | Will Opts.Option
  | Wont Opts.Option
  | Do Opts.Option
  | Dont Opts.Option
  | Subnegotiate Subnegotiation
  | Compression Bool
  | ZenithMudProtocol [T.Text]
  | TerminalType TType.Command (Maybe T.Text)
  | Environ Env.Command [EnvironmentVariable]
  | MudServerStatusProtocol [MsspVariable]

data Subnegotiation = Subnegotiation
  { subnegotiationBuffer :: [Word8],
    subnegotiationOption :: Opts.Option
  }

data Context = Context
  { options :: [Option],
    negotiationQueue :: [Negotiation],
    state :: State,
    flags :: [Flag],
    subnegotiation :: Subnegotiation,
    callback :: Event -> IO ()
  }

data NegotiationResult
  = Noop
  | SetRfc Q.State Q.State
  | SendNegotiation Cmds.Command
  | SendEvent Event

{-   | SetRfcSendNegotiation Q.State Q.State Cmds.Command
  | SetRfcSendEvent Q.State Q.State Event
  | SetRfcSendNegotiationAndEvent Q.State Q.State Cmds.Command Event -}
data TelnetException
  = BadValue String
  | BufferOverflow String
  | ProtocolViolation String
  | CompressionFailure String
  | OtherError String
  deriving (Show, Typ.Typeable)

instance Exception TelnetException

checkOption :: [Option] -> Opts.Option -> Bool -> Bool
checkOption [] _ _ = False
checkOption (Option {option = o, local = l, remote = r} : tail) option local
  | o == option = checkCmds
  | otherwise = checkOption tail option local
  where
    checkCmds :: Bool
    checkCmds
      | local && l == Cmds.Will = True
      | not local && r == Cmds.Do = True
      | otherwise = False

getRfc1143 :: Context -> Opts.Option -> Negotiation
getRfc1143 Context {negotiationQueue = queue} option =
  fromMaybe (Negotiation {negotiationOption = option, negotiationState = Q.No}) $
    find ((option ==) . negotiationOption) queue

setRfc1143 :: Context -> Opts.Option -> Q.State -> Q.State -> Context
setRfc1143 context@(Context {negotiationQueue = negotiationQueue, flags = flags}) option local remote = do
  let queue =
        negotiation :
        (snd $ partition ((option ==) . negotiationOption) negotiationQueue)
  case option of
    Opts.BinaryTransmission ->
      context
        { flags =
            setBinaryTxRx $
              filter
                ( \a ->
                    case a of
                      TransmitBinary -> False
                      ReceiveBinary -> False
                      _ -> True
                )
                flags,
          negotiationQueue = queue
        }
    _ -> context {negotiationQueue = queue}
  where
    negotiation :: Negotiation
    negotiation =
      Negotiation
        { negotiationOption = option,
          negotiationState = Q.create local remote
        }
    setBinaryTxRx :: [Flag] -> [Flag]
    setBinaryTxRx flags
      | local == Q.Yes = TransmitBinary : flags
      | remote == Q.Yes = ReceiveBinary : flags
      | otherwise = flags

send :: Context -> [Word8] -> IO ()
send Context {callback = callback} buffer = do
  callback $ SendData buffer

sendNegotiate :: Context -> Cmds.Command -> Opts.Option -> IO ()
sendNegotiate ctx (Cmds.Command cmd) (Opts.Option opt) = do
  let Cmds.Command iac = Cmds.InterpretAsCommand
  send ctx [iac, cmd, opt]

negotiate :: Context -> Opts.Option -> Context
negotiate
  context@( Context
              { options = options,
                state = state,
                flags = flags,
                callback = callback
              }
            )
  option
    | Proxy `notElem` flags =
      let negotiation = getRfc1143 context option
       in case negotiate' negotiation of
            [] -> context
            [Noop] -> context
            results ->
              let cxt =
                    case results of
                      [SetRfc a b] -> setRfc1143 context option a b
                      _ -> context
               in let _ =
                        map
                          ( \a ->
                              case a of
                                SendNegotiation a -> do
                                  sendNegotiate context a option
                                SendEvent a -> do
                                  callback a
                          )
                   in cxt
    | otherwise = do
      case state of
        ProcessWill ->
          let _ = callback $ Will option
           in context
        ProcessWont ->
          let _ = callback $ Wont option
           in context
        ProcessDo ->
          let _ = callback $ Do option
           in context
        ProcessDont ->
          let _ = callback $ Dont option
           in context
        _ -> context
    where
      negotiate' :: Negotiation -> [NegotiationResult]
      negotiate' Negotiation {negotiationState = negotiationState} =
        case state of
          ProcessWill ->
            case remoteState of
              Q.No ->
                if checkOption options option False
                  then
                    [ SetRfc localState Q.Yes,
                      SendNegotiation Cmds.Do,
                      SendEvent $ Will option
                    ]
                  else [SendNegotiation Cmds.Dont]
              Q.WantNo -> [SetRfc localState Q.No, SendEvent $ Wont option]
              Q.WantNoOpposite ->
                [SetRfc localState Q.Yes, SendEvent $ Will option]
              Q.WantYes -> [SetRfc localState Q.Yes, SendEvent $ Will option]
              Q.WantYesOpposite ->
                [ SetRfc localState Q.WantNo,
                  SendNegotiation Cmds.Dont,
                  SendEvent $ Will option
                ]
              _ -> [Noop]
          ProcessWont ->
            case remoteState of
              Q.Yes ->
                [ SetRfc localState Q.No,
                  SendNegotiation Cmds.Dont,
                  SendEvent $ Wont option
                ]
              Q.WantNo -> [SetRfc localState Q.No, SendEvent $ Wont option]
              Q.WantNoOpposite ->
                [SetRfc localState Q.WantYes, SendEvent $ Do option]
              Q.WantYes -> [SetRfc localState Q.No]
              Q.WantYesOpposite -> [SetRfc localState Q.No]
              _ -> [Noop]
          ProcessDo ->
            case localState of
              Q.No ->
                if checkOption options option True
                  then
                    [ SetRfc Q.Yes remoteState,
                      SendNegotiation Cmds.Will,
                      SendEvent $ Do option
                    ]
                  else [SendNegotiation Cmds.Wont]
              Q.WantNo -> [SetRfc Q.No remoteState, SendEvent $ Dont option]
              Q.WantNoOpposite ->
                [SetRfc Q.Yes remoteState, SendEvent $ Dont option]
              Q.WantYes -> [SetRfc Q.Yes remoteState, SendEvent $ Dont option]
              Q.WantYesOpposite ->
                [ SetRfc Q.WantNo remoteState,
                  SendNegotiation Cmds.Wont,
                  SendEvent $ Do option
                ]
              _ -> [Noop]
          ProcessDont ->
            case localState of
              Q.Yes ->
                [ SetRfc Q.No remoteState,
                  SendNegotiation Cmds.Wont,
                  SendEvent $ Dont option
                ]
              Q.WantNo -> [SetRfc Q.No remoteState, SendEvent $ Wont option]
              Q.WantNoOpposite ->
                [ SetRfc Q.WantYes remoteState,
                  SendNegotiation Cmds.Will,
                  SendEvent $ Will option
                ]
              Q.WantYes -> [SetRfc Q.No remoteState]
              Q.WantYesOpposite -> [SetRfc Q.No remoteState]
              _ -> [Noop]
          _ -> [Noop]
        where
          localState :: Q.State
          localState = Q.localState negotiationState
          remoteState :: Q.State
          remoteState = Q.remoteState negotiationState

processZenithMudProtocol :: (Event -> IO ()) -> Subnegotiation -> IO ()
processZenithMudProtocol callback subnegotiation@(Subnegotiation {subnegotiationBuffer = []}) =
  return ()
processZenithMudProtocol callback subnegotiation@(Subnegotiation {subnegotiationBuffer = buffer@(a : b)}) =
  if last buffer /= 0
    then throw $ ProtocolViolation "incomplete ZMP frame"
    else
      let strings =
            foldl
              ( \(strs, buf) byte ->
                  case byte of
                    0 ->
                      let value =
                            TE.decodeUtf8 $ BS.pack $ reverse (byte : buf)
                       in ((value : strs), [])
                    _ -> (strs, (byte : buf))
              )
              ([], [])
              buffer
       in do
            callback $ ZenithMudProtocol $ fst strings
            return ()

processTerminalType :: (Event -> IO ()) -> Subnegotiation -> IO ()
processTerminalType callback subnegotiation@(Subnegotiation {subnegotiationBuffer = []}) = do
  throw $ ProtocolViolation "incomplete TERMINAL-TYPE request"
processTerminalType callback subnegotiation@(Subnegotiation {subnegotiationBuffer = buffer@(a : b)}) =
  case TType.Command a of
    TType.Is ->
      let name = TE.decodeUtf8 $ BS.pack $ reverse (0 : buffer)
       in do callback $ TerminalType TType.Is (Just name)
    TType.Send -> do
      callback $ TerminalType TType.Send Nothing
    _ -> do
      throw $ ProtocolViolation "TERMINAL-TYPE request has invalid type"

processEnvironment :: (Event -> IO ()) -> Subnegotiation -> IO ()
processEnvironment callback subnegotiation@(Subnegotiation {subnegotiationBuffer = []}) = do
  return ()
processEnvironment callback subnegotiation@(Subnegotiation {subnegotiationBuffer = buffer@(a : b)})
  | Env.Command a `notElem` [Env.Is, Env.Send, Env.Info] = do
    throw $
      ProtocolViolation ("telopt " ++ show a ++ "subneg has invalid command")
  | length buffer == 1 = do callback $ Environ (Env.Command a) []
  | Env.Type (buffer !! 1) `notElem` [Env.Var, Env.User] = do
    throw $
      ProtocolViolation ("telopt " ++ show a ++ "subneg has invalid command")
  | Env.Type (last buffer) == Env.Esc = do return ()
  | otherwise = do
    let vars = readEnvVars b []
    callback $ Environ (Env.Command a) vars
  where
    readEnvVars :: [Word8] -> [EnvironmentVariable] -> [EnvironmentVariable]
    readEnvVars [] envVars = reverse envVars
    readEnvVars buffer@(a : b) envVars =
      let (c, var, vals) = readEnvVars' b [] Nothing Nothing
       in let d = createEnvVar (Env.Type a) var vals
           in readEnvVars c $ d : envVars
      where
        readEnvVars' ::
          [Word8] ->
          [Word8] ->
          Maybe T.Text ->
          Maybe [T.Text] ->
          ([Word8], T.Text, [T.Text])
        readEnvVars' [] tempbuffer variable values =
          cleanupEnviron [] tempbuffer variable values
        readEnvVars' buffer@(c : d) tempbuffer variable values =
          case Env.Type c of
            Env.Val ->
              let (_, var, vals) = cleanupEnviron [] tempbuffer variable values
               in readEnvVars' d [] (Just var) (Just vals)
            Env.Var -> cleanupEnviron buffer tempbuffer variable values
            Env.User -> cleanupEnviron buffer tempbuffer variable values
            Env.Esc -> readEnvVars' (tail d) tempbuffer variable values
            _ -> readEnvVars' d (c : tempbuffer) variable values

createEnvVar :: Env.Type -> T.Text -> [T.Text] -> EnvironmentVariable
createEnvVar Env.Var b c =
  EnvironmentVariable {globalVariable = b, globalValue = c}
createEnvVar Env.User b c =
  UserEnvironmentVariable {userVariable = b, userValue = c}
createEnvVar _ b c = throw $ BadValue "Invalid EnvironmentVariable type"

cleanupEnviron ::
  [Word8] ->
  [Word8] ->
  Maybe T.Text ->
  Maybe [T.Text] ->
  ([Word8], T.Text, [T.Text])
cleanupEnviron buffer tempbuffer (Just variable) values =
  let a = (TE.decodeUtf8 $ BS.pack $ reverse (0 : tempbuffer))
   in (buffer, variable, a : fromMaybe [] values)
cleanupEnviron buffer tempbuffer Nothing values =
  (buffer, TE.decodeUtf8 $ BS.pack $ reverse (0 : tempbuffer), [])

processMudServerStatusProtocol :: (Event -> IO ()) -> Subnegotiation -> IO ()
processMudServerStatusProtocol callback subnegotiation@(Subnegotiation {subnegotiationBuffer = []}) = do
  return ()
processMudServerStatusProtocol callback subnegotiation@(Subnegotiation {subnegotiationBuffer = buffer@(a : b)}) =
  case Mssp.Type a of
    Mssp.Variable ->
      throw $ ProtocolViolation "MSSP subnegotiation has invalid data"
    _ -> do
      let variables = readMsspVars buffer []
      callback $ MudServerStatusProtocol variables
  where
    readMsspVars :: [Word8] -> [MsspVariable] -> [MsspVariable]
    readMsspVars [] msspVariables = reverse msspVariables
    readMsspVars (a : b) msspVariables =
      let (buffer, variable, values) = readMsspVars' b [] Nothing Nothing
       in readMsspVars buffer $
            MsspVariable {msspVariable = variable, msspValue = values} :
            msspVariables
    readMsspVars' ::
      [Word8] ->
      [Word8] ->
      Maybe T.Text ->
      Maybe [T.Text] ->
      ([Word8], T.Text, [T.Text])
    readMsspVars' [] buffer variable values =
      cleanupEnviron [] buffer variable values
    readMsspVars' (a : b) buffer variable values =
      case Mssp.Type a of
        Mssp.Value ->
          let (_, var, vals) = cleanupEnviron [] buffer variable values
           in readMsspVars' b [] (Just var) (Just vals)
        Mssp.Variable -> cleanupEnviron (a : b) buffer variable values
        _ -> readMsspVars' b (a : buffer) variable values

subnegotiate :: (Event -> IO ()) -> Subnegotiation -> IO ()
subnegotiate
  callback
  subnegotiation@( Subnegotiation
                     { subnegotiationBuffer = sbBuffer,
                       subnegotiationOption = sbOpt
                     }
                   ) =
    case sbOpt of
      Opts.ZenithMudProtocol -> do
        processZenithMudProtocol callback subnegotiation
      Opts.TerminalType -> do
        processTerminalType callback subnegotiation
      Opts.Environment -> do
        processEnvironment callback subnegotiation
      Opts.NewEnvironment -> do
        processEnvironment callback subnegotiation
      Opts.MudServerStatusProtocol -> do
        processMudServerStatusProtocol callback subnegotiation
      _ -> do
        callback $ Subnegotiate subnegotiation
subnegotiate _ _ = do
  throw $ ProtocolViolation "Invalid Protocol State"

bufferByte :: Subnegotiation -> Word8 -> Either TelnetException Subnegotiation
bufferByte subnegotiation@(Subnegotiation {subnegotiationBuffer = sbBuffer}) byte = do
  if length sbBuffer >= 16384
    then Left $ BufferOverflow "subnegotiation buffer size limit reached"
    else Right subnegotiation {subnegotiationBuffer = byte : sbBuffer}

processData :: Context -> [Word8] -> IO Context
processData context [] = return context
-- todo: zlib stuff
processData context@(Context {callback = callback}) buffer = do
  let (cxt@(Context {state = state}), buf) =
        foldr processData' (context, []) buffer
  when
    (state == ProcessData)
    $ void $
      callback $ ReceiveData $ reverse buf
  return cxt
  where
    processData' :: Word8 -> (Context, [Word8]) -> (Context, [Word8])
    processData'
      byte
      ( context@( Context
                    { state = state,
                      flags = flags,
                      callback = callback,
                      subnegotiation = subnegotiation@(Subnegotiation {subnegotiationOption = sbOption})
                    }
                  ),
        buffer
        ) =
        case state of
          ProcessData ->
            case byte of
              (chkIac -> True) ->
                case buffer of
                  [] -> (context, buffer)
                  buffer -> do
                    let _ = callback $ ReceiveData $ reverse buffer
                    (context {state = ProcessIac}, [])
              (chkCrRt -> True) ->
                case buffer of
                  [] -> (context, buffer)
                  buffer -> do
                    let _ = callback $ ReceiveData $ reverse buffer
                    (context {state = ProcessEol}, [])
              _ -> (context, byte : buffer)
          ProcessEol ->
            case byte of
              (chkNwLn -> False) -> do
                let _ = callback $ ReceiveData [(toEnum . fromEnum) '\r']
                case byte of
                  0 -> (context {state = ProcessSkip}, buffer)
                  _ -> (context {state = ProcessData}, buffer)
          ProcessIac ->
            case Cmds.Command byte of
              Cmds.SubnegotiationBegin ->
                (context {state = ProcessSb}, buffer)
              Cmds.Will -> (context {state = ProcessWill}, buffer)
              Cmds.Wont -> (context {state = ProcessWont}, buffer)
              Cmds.Do -> (context {state = ProcessDo}, buffer)
              Cmds.Dont -> (context {state = ProcessDont}, buffer)
              Cmds.InterpretAsCommand -> do
                let _ = callback $ ReceiveData [byte]
                (context {state = ProcessData}, buffer)
              _ -> do
                let _ = callback $ InterpretAsCommand (Cmds.Command byte)
                (context {state = ProcessData}, buffer)
          ProcessWill ->
            let cxt = negotiate context (Opts.Option byte)
             in (cxt {state = ProcessData}, buffer)
          ProcessWont ->
            let cxt = negotiate context (Opts.Option byte)
             in (cxt {state = ProcessData}, buffer)
          ProcessDo ->
            let cxt = negotiate context (Opts.Option byte)
             in (cxt {state = ProcessData}, buffer)
          ProcessDont ->
            let cxt = negotiate context (Opts.Option byte)
             in (cxt {state = ProcessData}, buffer)
          ProcessSb ->
            let cxt =
                  context
                    { subnegotiation =
                        Subnegotiation
                          { subnegotiationBuffer = [],
                            subnegotiationOption = Opts.Option byte
                          }
                    }
             in (cxt, buffer)
          ProcessSbData ->
            case Cmds.Command byte of
              Cmds.InterpretAsCommand ->
                (context {state = ProcessSbDataIac}, buffer)
              (chkWillCmprss1 -> True) ->
                (context {state = ProcessSkip}, buffer)
              _ ->
                case bufferByte subnegotiation byte of
                  Right sub ->
                    ( context {subnegotiation = sub, state = ProcessData},
                      buffer
                    )
                  Left exc ->
                    case exc of
                      BufferOverflow message ->
                        (context {state = ProcessData}, buffer)
                      _ -> throw exc
          ProcessSbDataIac ->
            case Cmds.Command byte of
              Cmds.SubnegotiationEnd -> do
                let _ = subnegotiate callback subnegotiation
                (context {state = ProcessData}, buffer)
              Cmds.InterpretAsCommand ->
                let (Cmds.Command cmd) = Cmds.InterpretAsCommand
                 in case bufferByte subnegotiation cmd of
                      Right sub ->
                        ( context {subnegotiation = sub, state = ProcessSbData},
                          buffer
                        )
                      Left exc ->
                        case exc of
                          BufferOverflow message ->
                            (context {state = ProcessData}, buffer)
                          _ -> throw exc
              _ ->
                do
                  _ <- subnegotiate callback subnegotiation
                  cxt <- processData context {state = ProcessIac} [byte]
                  (cxt, buffer)
          ProcessSkip -> (context {state = ProcessData}, buffer)
        where
          chkIac :: Word8 -> Bool
          chkIac = (Cmds.InterpretAsCommand ==) . Cmds.Command

          chkCrRt :: Word8 -> Bool
          chkCrRt cmd =
            case chr $ fromIntegral cmd of
              '\r' -> NvtEndOfLine `elem` flags && ReceiveBinary `notElem` flags
              _ -> False
          chkNwLn :: Word8 -> Bool
          chkNwLn cmd =
            case chr $ fromIntegral cmd of
              '\n' -> True
              _ -> False
          chkWillCmprss1 :: Cmds.Command -> Bool
          chkWillCmprss1 cmd =
            case cmd of
              Cmds.Will ->
                case sbOption of
                  Opts.Compression1 -> True
                  _ -> False
              _ -> False

sendIac :: Context -> Word8 -> IO ()
sendIac context cmd = do
  let (Cmds.Command iac) = Cmds.InterpretAsCommand
  send context [iac, cmd]

processSend :: Context -> [Word8] -> IO ()
processSend context bytes = do
  loop bytes []
  where
    loop :: [Word8] -> [Word8] -> IO ()
    loop buffer databuffer =
      case buffer of
        [] -> do
          send context $ reverse databuffer
        a : b ->
          case Cmds.Command a of
            Cmds.InterpretAsCommand -> do
              sendIac context a
              loop (tail b) databuffer
            _ -> do
              loop b (a : databuffer)
