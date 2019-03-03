{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Validity.Comonad where

import Data.GenValidity
import Data.Data
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Utils
import Test.Validity.Functions

import Control.Comonad


comonadSpecOnValid ::
  forall (f :: * -> * ). (Eq (f Int), Show (f Int), Functor f, Typeable f, GenValid (f Int), Comonad f)
  => Spec
comonadSpecOnValid = comonadSpecWithInts @f genValid

comonadSpecWithInts ::
  forall (f :: * -> * ). (Eq (f Int), Show (f Int), Functor f, Typeable f, GenValid (f Int), Comonad f)
  => Gen (f Int)
  -> Spec
comonadSpecWithInts gen = comonadSpecOnGens gen (unwords [nameOf @f, "of ints"])

extendTypeStr :: forall (f :: * -> *). (Typeable f) => String
extendTypeStr = unwords ["extend", "::", "(", nameOf @f, "->", "a", ")", "->", nameOf @f, "->", nameOf @f, "a"]

extractTypeStr :: forall (f :: * -> *). (Typeable f) => String
extractTypeStr = unwords ["extract", "::", nameOf @f, "a", "->", "a"]

duplicateStr :: forall (f :: * -> *). (Typeable f) => String
duplicateStr = unwords ["duplicate", "::", nameOf @f, "a", "->", nameOf @f, "(", nameOf @f, "a", ")"]

comonadSpecOnGens ::
  forall (f :: * -> *) (a :: *).
       ( Show a
       , Show (f a)
       , Eq (f a)
       , Functor f
       , Comonad f
       , Typeable f
       , Typeable a
       )
  =>
  Gen (f a)
  -> String
  -> Spec
comonadSpecOnGens gen genname =
  parallel $
    describe ("Comonad " <> nameOf @f) $ do
      describe (unwords [extendTypeStr @f, "and", extractTypeStr @f]) $ do
        it (unwords ["satisfy extract extract = id for", genDescr @(f a) genname]) $
          equivalentOnGen (extend @f extract) (id @(f a)) gen shrinkNothing
        it (unwords ["satisfy extract . extend f = f for", genDescr @(f a) genname]) $
          equivalentOnGen (extract @f . extend id) (id @(f a)) gen shrinkNothing
        it (unwords ["satisfy extend f . extend g = extend (f . extend g) for", genDescr @(f a) genname]) $
          equivalentOnGen (extract . extract . extend @f id . extend @f id) (extract . extract . extend @f (id . extend @f id)) gen shrinkNothing
      describe (unwords [duplicateStr @f, "and", extractTypeStr @f]) $ do
        it (unwords ["satisfy extract . duplicate = id for", genDescr @(f a) genname]) $
          equivalentOnGen (extract @f . duplicate) (id @(f a)) gen shrinkNothing
        it (unwords ["satisfy fmap extract . duplicate = id for", genDescr @(f a) genname]) $
          equivalentOnGen (fmap (extract @f) . duplicate) (id @(f a)) gen shrinkNothing
        it (unwords ["satisfy duplicate . duplicate = fmap duplicate . duplicate for", genDescr @(f a) genname]) $
          equivalentOnGen (extract . extract . duplicate @f . duplicate) (extract . extract . fmap duplicate . duplicate @f) gen shrinkNothing
      describe (unwords [duplicateStr @f, "and", extendTypeStr @f]) $ do
        it (unwords ["satisfy extend f = fmap f . duplicate for", genDescr @(f a) genname]) $
          equivalentOnGen (extract . extend @f id) (extract . fmap id . duplicate @f) gen shrinkNothing
        it (unwords ["satisfy duplicate = extend id for", genDescr @(f a) genname]) $
          equivalentOnGen (extract . duplicate @f) (extract . extend @f id) gen shrinkNothing
        it (unwords ["satisfy fmap f = extend (f . extract) for", genDescr @(f a) genname]) $
          equivalentOnGen (fmap id) (extend (id . extract @f)) gen shrinkNothing
