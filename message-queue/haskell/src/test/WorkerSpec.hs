module WorkerSpec
( spec )
where

import Test.Hspec

import System.Clock (TimeSpec(..))
import Jmcgmqp.Worker

spec :: Spec
spec = do
  describe "newStart" $ do
    it "works" $ do
      newStart 0 `shouldBe` Nothing
      newStart 1 `shouldBe` Nothing
      newStart 2 `shouldBe` Just 3
      newStart 3 `shouldBe` Just 5

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
