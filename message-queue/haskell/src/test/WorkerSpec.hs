module WorkerSpec
( spec )
where

import Test.Hspec (Spec, describe, it, shouldBe)

import System.Clock (TimeSpec(TimeSpec))
import Jmcgmqp.Worker (newResults, newWorkerResult, Results(messagesPerSecond))

spec :: Spec
spec = do
  describe "Results" $ do
    it "calculates mps 1" $ do
      let rs = newResults [newWorkerResult 1 10 $ TimeSpec 1 0]
      rs.messagesPerSecond `shouldBe` 10

    it "calculates mps 2" $ do
      let rs = newResults [
             newWorkerResult 1 10 $ TimeSpec 1 0,
             newWorkerResult 2 10 $ TimeSpec 1 0
             ]
      rs.messagesPerSecond `shouldBe` 20

    it "calculates mps 3" $ do
      let rs = newResults [
             newWorkerResult 1 10 $ TimeSpec 1 0,
             newWorkerResult 2 10 $ TimeSpec 1 0,
             newWorkerResult 3 10 $ TimeSpec 1 0,
             newWorkerResult 4 10 $ TimeSpec 1 0
             ]
      rs.messagesPerSecond `shouldBe` 40

    it "calculates mps 4" $ do
      let rs = newResults [
             newWorkerResult 1 10 $ TimeSpec 1 0,
             newWorkerResult 2 20 $ TimeSpec 2 0
             ]
      rs.messagesPerSecond `shouldBe` 20
