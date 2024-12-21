module AlgorithmSpec
( spec )
where

import Test.Hspec

import System.Clock (TimeSpec(..))
import Jmcgmqp.Algorithm

spec :: Spec
spec = do
  describe "newStart" $ do
    it "works" $ do
      newStart 0 `shouldBe` Nothing
      newStart 1 `shouldBe` Nothing
      newStart 2 `shouldBe` Just 3
      newStart 3 `shouldBe` Just 5

