-- Semantic model

module FlowModel where

import qualified Data.Time.Clock
import qualified Data.Time.Format

-- data SideEffectT a
--   = DeleteFlow (() -> SideEffect a)
--   | ReadFlow ((Maybe String) -> SideEffect a)
--   | WriteFlow String (() -> SideEffect a)
--   | WriteLedger String (() -> SideEffect a)
--   | GetTime (Data.Time.Clock.UTCTime -> SideEffect a)
--   | Log String (() -> SideEffect a)
--   | NoSideEffects a
--
-- type SideEffect a = (SideEffectT a, (a -> SideEffectT a))

data SideEffect a
  = NoSideEffects a
  | DeleteFlow (() -> SideEffect a)
  | ReadFlow ((Maybe String) -> SideEffect a)
  | WriteFlow String (() -> SideEffect a)
  | WriteLedger String (() -> SideEffect a)
  | GetTime (Data.Time.Clock.UTCTime -> SideEffect a)
  | Log String (() -> SideEffect a)

noEffect :: a -> SideEffect a
noEffect a = NoSideEffects a

deleteFlow :: SideEffect ()
deleteFlow = DeleteFlow NoSideEffects

readFlow :: SideEffect (Maybe String)
readFlow = ReadFlow NoSideEffects

writeFlow :: String -> SideEffect ()
writeFlow s = WriteFlow s NoSideEffects

writeLedger :: String -> SideEffect ()
writeLedger s = WriteLedger (s ++ "\n") NoSideEffects

getTime :: SideEffect Data.Time.Clock.UTCTime
getTime = GetTime NoSideEffects

logLine :: String -> SideEffect ()
logLine s = Log (s ++ "\n") NoSideEffects

-- do notation support

andThen :: SideEffect a -> (a -> SideEffect b) -> SideEffect b
andThen (NoSideEffects a) f = f a
andThen (DeleteFlow next) f =
  DeleteFlow (\() -> andThen (next ()) f)
andThen (ReadFlow next) f =
  ReadFlow (\s -> andThen (next s) f)
andThen (WriteFlow s next) f =
  WriteFlow s (\() -> andThen (next ()) f)
andThen (WriteLedger s next) f =
  WriteLedger s (\() -> andThen (next ()) f)
andThen (GetTime next) f =
  GetTime (\t -> andThen (next t) f)
andThen (Log s next) f =
  Log s (\() -> andThen (next ()) f)

instance Functor SideEffect where
  fmap f (NoSideEffects a) = NoSideEffects (f a) -- (a -> b) -> f a -> f b

instance Applicative SideEffect where
  pure = noEffect
  (NoSideEffects f) <*> (NoSideEffects arg) = NoSideEffects (f arg)

instance Monad SideEffect where
  return = noEffect
  a >>= b = andThen a b

-- Program

data Action
  = Start
  | Stop
  | Fail
  | Help

runAction :: Action -> SideEffect ()
runAction Start =
  do
    oldFlow <- readFlow
    case oldFlow of
      Just timeStr ->
        logLine ("There already exists an ongoing flow started at " ++ timeStr ++ ".")
      Nothing ->
        do
          time <- getTime
          writeFlow (formatTime time)
          logLine "Flow started."

runAction Stop =
  do
    currentFlow <- readFlow
    case currentFlow of
      Just startTimeStr ->
        case tryParseTime startTimeStr of
          Just startTime -> finishFlow startTime
          Nothing -> logLine "Failed to parse flow file."
      Nothing ->
        logLine "No ongoing flow."

runAction Fail = deleteFlow

runAction Help =
  logLine "Usage:" >>
  logLine "flow [start] - start a new flow" >>
  logLine "flow d[one] - record a succesful flow" >>
  logLine "flow f[ail] - fail the current flow"

finishFlow :: Data.Time.Clock.UTCTime -> SideEffect ()
finishFlow startTime =
  do
    endTime <- getTime
    let duration = calculateDuration startTime endTime
    let csv = (formatTime startTime) ++ "," ++
              (formatTime endTime) ++ "," ++
              (show duration)
    writeLedger csv
    deleteFlow
    logLine ("Flow lasted for " ++ (show duration) ++ " seconds.")

calculateDuration :: Data.Time.Clock.UTCTime -> Data.Time.Clock.UTCTime -> Int
calculateDuration startTime endTime =
  round (Data.Time.Clock.diffUTCTime endTime startTime)

formatTime :: Data.Time.Clock.UTCTime -> String
formatTime time =
  Data.Time.Format.formatTime locale iso8601 time

tryParseTime :: String -> Maybe Data.Time.Clock.UTCTime
tryParseTime timeStr =
  Data.Time.Format.parseTimeM True locale iso8601 timeStr

locale :: Data.Time.Format.TimeLocale
locale = Data.Time.Format.defaultTimeLocale

iso8601 :: String
iso8601 = "%Y-%m-%dT%H:%M:%S%z"


