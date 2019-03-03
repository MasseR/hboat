module Main where

import Test.Hspec

import qualified Test.Data.Zipper

main :: IO ()
main = hspec $ do
  Test.Data.Zipper.spec
