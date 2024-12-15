module Main
( main )
where

import Test.Hspec
import Test.QuickCheck

import System.Clock (TimeSpec(..))
import Jmcgmqp.Worker

main = hspec spec

spec = do
  describe "newStart" $ do
    it "works" $ do
      newStart 0 `shouldBe` Nothing
      newStart 1 `shouldBe` Nothing
      newStart 2 `shouldBe` (Just 3)
      newStart 3 `shouldBe` (Just 5)

  describe "Results" $ do
    it "calculates mps" $ do
      let rs = newResults [newWorkerResult 1 10 $ TimeSpec 1 0]
      rs.messagesPerSecond `shouldBe` 10

      let rs = newResults [
             newWorkerResult 1 10 $ TimeSpec 1 0,
             newWorkerResult 2 10 $ TimeSpec 1 0
             ]
      rs.messagesPerSecond `shouldBe` 20

      let rs = newResults [
             newWorkerResult 1 10 $ TimeSpec 1 0,
             newWorkerResult 2 10 $ TimeSpec 1 0,
             newWorkerResult 3 10 $ TimeSpec 1 0,
             newWorkerResult 4 10 $ TimeSpec 1 0
             ]
      rs.messagesPerSecond `shouldBe` 40

      let rs = newResults [
             newWorkerResult 1 10 $ TimeSpec 1 0,
             newWorkerResult 2 20 $ TimeSpec 2 0
             ]
      rs.messagesPerSecond `shouldBe` 20
