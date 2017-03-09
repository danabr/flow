{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import FlowModel
import Control.Exception (assert)
import qualified Data.Maybe
import qualified Data.Time.Clock
import qualified Data.Time.Format
import Control.Monad.State
import Control.Monad.Reader

data MockWorld = MockWorld { mockTime :: Data.Time.Clock.UTCTime
                           , mockFlow :: (Maybe String)
                           , mockLedger :: [String]
                           } deriving (Eq, Show)

newtype TestMonad a = TestMonad (State MockWorld a)
  deriving (Functor, Applicative, Monad, MonadState MockWorld)

instance MTime TestMonad where
  getTime = do
    world <- get
    let time = mockTime world
    return time

instance MLog TestMonad where
  logLine _ = pure ()

instance MFlowFS TestMonad where
  deleteFlow = do
    world <- get
    let newWorld = world { mockFlow = Nothing }
    put newWorld
    return ()

  readFlow = do
    world <- get
    let flow = mockFlow world
    return flow

  writeFlow str = do
    world <- get
    let newWorld = world { mockFlow = Just str }
    put newWorld
    return ()

  writeLedger str = do
    world <- get
    let ledger = mockLedger world
    let newLedger = str : ledger
    let newWorld = world { mockLedger = newLedger }
    put newWorld
    return ()

testRunAction action world = 
  let (TestMonad m) = testAction action 
      (_, endWorld) = runState m world in
  endWorld

testAction :: Action -> TestMonad ()
testAction = runAction

noFlowWorld :: MockWorld
noFlowWorld =
  let Just time = Data.Time.Format.parseTimeM True locale iso8601 startOfTime in
  MockWorld { mockTime = time
            , mockFlow = Nothing
            , mockLedger = []
            }

startOfTime :: String
startOfTime = "0-01-01T00:00:00+0000"

type Test = () -> ()

startNoFlowTest :: Test
startNoFlowTest =
  let endWorld = testRunAction Start noFlowWorld in
  assert (null (mockLedger endWorld)) $
  assert ((Just startOfTime) == (mockFlow endWorld))

startAlreadyStartedTest :: Test
startAlreadyStartedTest =
  let startedWorld = testRunAction Start noFlowWorld
      futureWorld = stepTime startedWorld 1
      endWorld = testRunAction Start futureWorld in
  assert ((Just startOfTime) == (mockFlow endWorld))

stopTest :: Test
stopTest =
  let startedWorld = testRunAction Start noFlowWorld in
  let futureWorld = stepTime startedWorld 1 in
  let endWorld = testRunAction Stop futureWorld in
  assert (Data.Maybe.isNothing (mockFlow endWorld)) $
  let [entry] = mockLedger endWorld in
  assert (entry == "0-01-01T00:00:00+0000,0-01-01T00:00:01+0000,1")

stopNoFlowTest :: Test
stopNoFlowTest =
  let startWorld = noFlowWorld in
  let endWorld = testRunAction Stop startWorld in
  assert (endWorld == startWorld)

stopBadFlowTimeTest :: Test
stopBadFlowTimeTest =
  let startWorld = noFlowWorld { mockFlow = Just "garbage" } in
  let endWorld = testRunAction Stop startWorld in
  assert (endWorld == startWorld)

failTest :: Test
failTest =
  let startedWorld = testRunAction Start noFlowWorld in
  let failedWorld = testRunAction Fail startedWorld in
  assert (Data.Maybe.isNothing (mockFlow failedWorld)) $
  assert (null (mockLedger failedWorld))

failNoFlowTest :: Test
failNoFlowTest =
  let startWorld = noFlowWorld in
  let endWorld = testRunAction Fail startWorld in
  assert (endWorld == startWorld)

helpTest :: Test
helpTest =
  let startWorld = noFlowWorld in
  let endWorld = testRunAction Help startWorld in
  assert (endWorld == startWorld)

runTests :: ()
runTests =
  startNoFlowTest $
  startAlreadyStartedTest $
  stopTest $
  stopNoFlowTest $
  stopBadFlowTimeTest $
  failTest $
  failNoFlowTest $
  helpTest ()

-- Test helpers

stepTime :: MockWorld -> Data.Time.Clock.NominalDiffTime -> MockWorld
stepTime world seconds =
  let flowTime = mockTime world in
  let futureTime = Data.Time.Clock.addUTCTime seconds flowTime in
  world { mockTime = futureTime }
