module Main
( main )
where

import Test.Hspec
import Test.QuickCheck
import Jmcgmqp.Worker

main = hspec spec

spec = describe "newStart" $ do
  it "works" $ do
    newStart 0 `shouldBe` Nothing
    newStart 1 `shouldBe` Nothing
    newStart 2 `shouldBe` (Just 3)
    newStart 3 `shouldBe` (Just 5)
