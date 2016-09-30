{-# LANGUAGE NamedFieldPuns #-}

import FlowModel
import Control.Exception (assert)
import qualified Data.Maybe
import qualified Data.Time.Clock
import qualified Data.Time.Format

data MockWorld = MockWorld { mockTime :: Data.Time.Clock.UTCTime
                           , mockFlow :: (Maybe String)
                           , mockLedger :: [String]
                           } deriving (Eq, Show)

noFlowWorld :: MockWorld
noFlowWorld =
  let Just time = Data.Time.Format.parseTimeM True locale iso8601 startOfTime in
  MockWorld { mockTime = time
            , mockFlow = Nothing
            , mockLedger = []
            }

startOfTime :: String
startOfTime = "0-01-01T00:00:00+0000"

testRunSideEffects :: SideEffect a -> MockWorld -> MockWorld
testRunSideEffects (NoSideEffects _) world@(MockWorld{mockLedger}) =
    world { mockLedger = (reverse mockLedger) }
testRunSideEffects (DeleteFlow next) world =
    let newWorld = world { mockFlow = Nothing } in
    testRunSideEffects (next ()) newWorld
testRunSideEffects (ReadFlow next) world@(MockWorld {mockFlow=mockFlow}) =
    testRunSideEffects (next mockFlow) world
testRunSideEffects (WriteFlow s next) world =
    let newWorld = world { mockFlow = Just s } in
    testRunSideEffects (next ()) newWorld
testRunSideEffects (WriteLedger s next) world@(MockWorld {mockLedger=mockLedger}) =
    let newWorld = world { mockLedger = (s : mockLedger) } in
    testRunSideEffects (next ()) newWorld
testRunSideEffects (GetTime next) world@(MockWorld {mockTime=mockTime}) =
    testRunSideEffects (next mockTime) world
testRunSideEffects (Log _ next) world =
    testRunSideEffects (next ()) world

type Test = () -> ()

startNoFlowTest :: Test
startNoFlowTest =
  let startFlow = runAction Start in
  let endWorld = testRunSideEffects startFlow noFlowWorld in
  assert (null (mockLedger endWorld)) $
  assert ((Just startOfTime) == (mockFlow endWorld))

startAlreadyStartedTest :: Test
startAlreadyStartedTest =
  let startFlow = runAction Start in
  let startedWorld = testRunSideEffects startFlow noFlowWorld in
  let futureWorld = stepTime startedWorld 1 in
  let endWorld = testRunSideEffects startFlow futureWorld in
  assert ((Just startOfTime) == (mockFlow endWorld))

stopTest :: Test
stopTest =
  let startedWorld = testRunSideEffects (runAction Start) noFlowWorld in
  let futureWorld = stepTime startedWorld 1 in
  let endWorld = testRunSideEffects (runAction Stop) futureWorld in
  assert (Data.Maybe.isNothing (mockFlow endWorld)) $
  let [entry] = mockLedger endWorld in
  assert (entry == "0-01-01T00:00:00+0000,0-01-01T00:00:01+0000,1\n")

stopNoFlowTest :: Test
stopNoFlowTest =
  let startWorld = noFlowWorld in
  let endWorld = testRunSideEffects (runAction Stop) startWorld in
  assert (endWorld == startWorld)

stopBadFlowTimeTest :: Test
stopBadFlowTimeTest =
  let startWorld = noFlowWorld { mockFlow = Just "garbage" } in
  let endWorld = testRunSideEffects (runAction Stop) startWorld in
  assert (endWorld == startWorld)

failTest :: Test
failTest =
  let startFlow = runAction Start in
  let startedWorld = testRunSideEffects startFlow noFlowWorld in
  let failedWorld = testRunSideEffects (runAction Fail) startedWorld in
  assert (Data.Maybe.isNothing (mockFlow failedWorld)) $
  assert (null (mockLedger failedWorld))

failNoFlowTest :: Test
failNoFlowTest =
  let startWorld = noFlowWorld in
  let endWorld = testRunSideEffects (runAction Fail) startWorld in
  assert (endWorld == startWorld)

helpTest :: Test
helpTest =
  let startWorld = noFlowWorld in
  let endWorld = testRunSideEffects (runAction Help) startWorld in
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

main :: IO ()
main =
  putStr ("Tests passed: " ++ (show runTests))
