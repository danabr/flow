{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- flow implemented using a DSL that aids testing.

import FlowModel
import qualified Control.Exception
import Control.Monad.IO.Class
import qualified Data.Maybe
import qualified Data.Time.Clock
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath

newtype FlowIO a = FlowIO (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runFlowIO :: FlowIO a -> IO a
runFlowIO (FlowIO a) = a

main :: IO ()
main =
  do
    args <- System.Environment.getArgs
    let action = (runAction (argsToAction args)) :: FlowIO ()
    runFlowIO action

instance MTime FlowIO where
  getTime = liftIO $ Data.Time.Clock.getCurrentTime

instance MLog FlowIO where
  logLine = liftIO . putStrLn

instance MFlowFS FlowIO where
  deleteFlow = do
    flowFile <- getFlowFile
    liftIO $ System.Directory.removeFile flowFile

  readFlow = do
    flowFile <- getFlowFile
    tryReadFile flowFile

  writeFlow timeStr = do
    flowFile <- getFlowFile
    liftIO $ writeFile flowFile timeStr

  writeLedger row = do
    ledger <- getLedgerFile
    liftIO $ appendFile ledger (row ++ "\n")

getFlowFile :: FlowIO String
getFlowFile = getPath ".flow"

getLedgerFile :: FlowIO String
getLedgerFile = getPath "flow_ledger.csv"

getPath :: String -> FlowIO String
getPath file =
  do
    home <- liftIO $ System.Environment.lookupEnv "HOME"
    let dir = Data.Maybe.fromMaybe "/" home
    let path = System.FilePath.joinPath [dir, file]
    return path

tryReadFile :: String -> FlowIO (Maybe String)
tryReadFile path =
  do
    res <- liftIO $ Control.Exception.try (readFile path)
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
