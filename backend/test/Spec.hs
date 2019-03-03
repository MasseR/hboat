module Main where

import Test.Hspec

import qualified Test.Data.Zipper

main :: IO ()
main = hspec
  Test.Data.Zipper.spec
