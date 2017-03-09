module FlowModel where

import qualified Data.Time.Clock
import qualified Data.Time.Format

class Monad m => MTime m where
  getTime :: m Data.Time.Clock.UTCTime

class Monad m => MLog m where
  logLine :: String -> m ()

class Monad m => MFlowFS m where
  deleteFlow :: m ()
  readFlow :: m (Maybe String)
  writeFlow :: String -> m ()
  writeLedger :: String -> m ()

data Action
  = Start
  | Stop
  | Fail
  | Help

runAction :: (MFlowFS m, MLog m, MTime m) => Action -> m ()
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

finishFlow :: (MLog m, MFlowFS m, MTime m) => Data.Time.Clock.UTCTime -> m ()
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

printHelp :: MLog m => m ()
printHelp =
  logLine "Usage:" >>
  logLine "flow [start] - start a new flow" >>
  logLine "flow d[one] - record a succesful flow" >>
  logLine "flow f[ail] - fail the current flow"
  
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
