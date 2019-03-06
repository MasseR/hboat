{-# LANGUAGE TypeApplications #-}
module Test.Data.Zipper where

import           Data.Zipper
import           Test.Hspec
import           Test.Validity.Functor
import           Test.Validity.Applicative
import           Test.Validity.Comonad
import           Test.Validity

spec :: Spec
spec = describe "Data.Zipper" $ do
  functorSpecOnValid @Zipper
  applicativeSpecOnValid @Zipper
  comonadSpecOnValid @Zipper
  it "foldMap (:[]) . fromList = foldMap (:[]) . id" $
    equivalentOnValid (foldMap (:[]) . fromList @Int . (0:)) (foldMap (:[]) . (0:))
  it "foldMap (:[]) . tugs t . fromList = foldMap (:[]) . id" $
    equivalentOnValids2 (\t -> foldMap (:[]) . flip tugs (t :: [Dir]) . fromList @Int . (0:)) (\_ -> foldMap (:[]) . (0:))
