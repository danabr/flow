-- flow implemented using a DSL that aids testing.

import FlowModel
import qualified Control.Exception
import qualified Data.Maybe
import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath


-- Real implemention
sideEffectsToIO :: SideEffect () -> IO ()
sideEffectsToIO (NoSideEffects x) = return x

sideEffectsToIO (DeleteFlow next) =
  do
    flowFile <- getFlowFile
    System.Directory.removeFile flowFile
    sideEffectsToIO (next ())

sideEffectsToIO (ReadFlow next) =
  do
    flowFile <- getFlowFile
    res <- tryReadFile flowFile
    sideEffectsToIO (next res)

sideEffectsToIO (WriteFlow timeStr next) =
  do
    flowFile <- getFlowFile
    writeFile flowFile timeStr
    sideEffectsToIO (next ())

sideEffectsToIO (WriteLedger row next) =
  do
    ledger <- getLedgerFile
    appendFile ledger row
    sideEffectsToIO (next ())
sideEffectsToIO (GetTime next) =
  do
    time <- Data.Time.Clock.getCurrentTime
    sideEffectsToIO (next time)
sideEffectsToIO (Log str next) =
  do
    putStr str
    sideEffectsToIO (next ())

main :: IO ()
main =
  do
    args <- System.Environment.getArgs
    sideEffectsToIO (runAction (argsToAction args))

getFlowFile :: IO String
getFlowFile = getPath ".flow"

getLedgerFile :: IO String
getLedgerFile = getPath "flow_ledger.csv"

getPath :: String -> IO String
getPath file =
  do
    home <- System.Environment.lookupEnv "HOME"
    let dir = Data.Maybe.fromMaybe "/" home
    return (System.FilePath.joinPath [dir, file])

tryReadFile :: String -> IO (Maybe String)
tryReadFile path =
  do
    res <- Control.Exception.try (readFile path)
    case res of
      Right contents                           -> return (Just contents)
      Left (Control.Exception.SomeException _) -> return Nothing

argsToAction :: [String] -> Action
argsToAction [] = Start
argsToAction ["start"] = Start
argsToAction ["d"] = Stop
argsToAction ["done"] = Stop
argsToAction ["f"] = Fail
argsToAction ["fail"] = Fail
argsToAction _ = Help
