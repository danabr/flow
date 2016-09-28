import qualified Control.Exception
import qualified Data.Maybe
import qualified Data.Time.Clock
import qualified Data.Time.Format
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath

-- Need to keep track of
-- * flow file
-- * ledger file
-- * time
-- CLI args (System.Environment.getArgs)
--
data Action
  = Start
  | Stop
  | Fail
  | Help

main :: IO ()
main =
  do
    args <- System.Environment.getArgs
    runAction (argsToAction args) 

argsToAction [] = Start
argsToAction ["start"] = Start
argsToAction ["d"] = Stop
argsToAction ["done"] = Stop
argsToAction ["f"] = Fail
argsToAction ["fail"] = Fail
argsToAction _ = Help

runAction Start =
  do
    flowFile <- getFlowFile
    oldFlow <- readFlowFile flowFile
    case oldFlow of
      Just c ->
        putStr ("There already exists an ongoing flow started at " ++ c ++ "\n")
      Nothing ->
        do
          time <- getTimeAsISOStr
          writeFile flowFile time
          putStr "Flow started\n"
runAction Stop =
  do
    flowFile <- getFlowFile
    flow <- readFlowFile flowFile
    case flow of
      Just c  ->
        do
          let maybeStartTime = tryParseTime c
          case maybeStartTime of
            Just startTime ->
              do
                endTime <- Data.Time.Clock.getCurrentTime
                let duration = toInteger (round (Data.Time.Clock.diffUTCTime endTime startTime))
                let csv = c ++ "," ++ (formatTime endTime) ++ "," ++ (show duration) ++ "\n"
                ledger <- getLedgerFile
                appendFile ledger csv
                System.Directory.removeFile flowFile
                putStr ("Flow lasted for " ++ (show duration) ++ " seconds.\n")
            Nothing ->
              putStr "Failed to parse flow file.\n"
      Nothing ->
        putStr "No ongoing flow.\n"
runAction Fail =
  do
    flowFile <- getFlowFile
    System.Directory.removeFile flowFile
runAction Help =
  putStr help

getFlowFile = getPath ".flow"

getLedgerFile = getPath "flow_ledger.csv"

getPath file =
  do
    home <- System.Environment.lookupEnv "HOME"
    let dir = Data.Maybe.fromMaybe "/" home
    return (System.FilePath.joinPath [dir, file])

readFlowFile path =
  do
    res <- Control.Exception.try (readFile path)
    case res of
      Right contents                           -> return (Just contents)
      Left (Control.Exception.SomeException _) -> return Nothing

tryParseTime :: String -> Maybe Data.Time.Clock.UTCTime
tryParseTime timeStr =
  Data.Time.Format.parseTimeM True locale iso8601 timeStr 

getTimeAsISOStr =
  do
    time <- Data.Time.Clock.getCurrentTime
    return (formatTime time)

formatTime time =
  Data.Time.Format.formatTime locale iso8601 time

locale = Data.Time.Format.defaultTimeLocale

iso8601 = "%Y-%m-%dT%H:%M:%S%z"

help =
  "Usage:\n" ++
  "flow [start] - start a new flow\n" ++
  "flow d[one] - record a succesful flow\n" ++
  "flow f[ail] - fail the current flow\n"
