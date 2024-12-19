module Main
( main )
where

import Test.Hspec
import Test.QuickCheck

import WorkerSpec

main = hspec $ describe "Worker" WorkerSpec.spec
