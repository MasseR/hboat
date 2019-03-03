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

instance Foldable Zipper where
  foldMap f Zipper{..} = foldl' (\acc x -> f x <> acc) mempty lefts <> f value <> foldMap f rights

fromList :: [a] -> Zipper a
fromList [] = error "No empty zipper"
fromList (x:xs) = Zipper [] x xs

data Dir = L | R deriving (Generic, Show, Eq, GenUnchecked, GenValid, Validity)

tug :: Zipper a -> Dir -> Zipper a
tug zipper dir =
  case (zipper,dir) of
       (z@Zipper{lefts=[]}, L) -> z
       (Zipper{lefts=(l:ls),value,rights}, L) -> Zipper ls l (value:rights)
       (z@Zipper{rights=[]}, R) -> z
       (Zipper{lefts,value,rights=(r:rs)}, R) -> Zipper (value:lefts) r rs

tugs :: Foldable t => Zipper a -> t Dir -> Zipper a
tugs = foldl' tug

instance Comonad Zipper where
  extract Zipper{value} = value
  -- extend f Zipper{..} = Zipper (fmap (f . pure) lefts) (f (pure value)) (fmap (f . pure) rights)
  duplicate z@Zipper{..} = Zipper (drop 1 $ scanl' (\acc _ -> acc `tug` L) z lefts) z (drop 1 $ scanl' (\acc _ -> acc `tug` R) z rights)


left :: Zipper a -> a
left Zipper{..} = fromMaybe value . listToMaybe $ lefts

leftward :: Zipper a -> Zipper a
leftward = extend left

rightward :: Zipper a -> Zipper a
rightward = extend right

right :: Zipper a -> a
right Zipper{..} = fromMaybe value . listToMaybe $ rights

