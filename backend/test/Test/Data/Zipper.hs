{-# LANGUAGE TypeApplications #-}
module Test.Data.Zipper where

import           HBoat.Data.Zipper
import           Test.Hspec
import           Test.Validity.Functor
import           Test.Validity.Applicative
import           Test.Validity.Comonad

spec :: Spec
spec = describe "Data.Zipper" $ do
  functorSpecOnValid @Zipper
  applicativeSpecOnValid @Zipper
  comonadSpecOnValid @Zipper
