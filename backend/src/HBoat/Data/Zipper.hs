{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
module HBoat.Data.Zipper where

import CustomPrelude
import Control.Comonad
import Data.GenValidity

data Zipper a = Zipper { lefts :: [a]
                       , value :: !a
                       , rights :: [a] }
              deriving (Eq, Generic, Show, Functor, GenUnchecked, GenValid, Validity)

instance Applicative Zipper where
  pure a = Zipper [] a []
  Zipper{value=f} <*> z = fmap f z

instance Comonad Zipper where
  extract Zipper{value} = value
  duplicate Zipper{..} = Zipper (fmap pure lefts) (Zipper [] value []) (fmap pure rights)

left :: Zipper a -> a
left Zipper{..} = fromMaybe value . listToMaybe $ lefts

leftward :: Zipper a -> Zipper a
leftward = extend left

rightward :: Zipper a -> Zipper a
rightward = extend right

right :: Zipper a -> a
right Zipper{..} = fromMaybe value . listToMaybe $ rights

