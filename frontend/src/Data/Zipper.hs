{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
module Data.Zipper where

import           Control.Comonad

data Zipper a = Zipper [a] !a [a] deriving (Functor, Traversable, Show, Eq)

instance Semigroup a => Semigroup (Zipper a) where
  Zipper l a r <> Zipper l' a' r' = Zipper l a (r <> reverse l' <> pure a' <> r')

instance Foldable Zipper where
  foldMap f (Zipper ls a rs) = foldMap f (reverse ls) <> f a <> foldMap f rs

instance Applicative Zipper where
  pure a = Zipper [] a []
  (Zipper _ f _) <*> z = fmap f z

instance Comonad Zipper where
  extract (Zipper _ a _) = a
  extend f (Zipper ls a rs) = Zipper (map (f . pure) ls) (f (pure a)) (map (f . pure) rs)

left, right :: Zipper a -> Zipper a
left = \case
  Zipper [] a r -> Zipper [] a r
  Zipper (l:ls) a rs -> Zipper ls l (a:rs)
right = \case
  Zipper l a [] -> Zipper l a []
  Zipper ls a (r:rs) -> Zipper (a:ls) r rs
