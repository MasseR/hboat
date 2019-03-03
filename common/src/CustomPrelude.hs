{-# LANGUAGE NoImplicitPrelude #-}
module CustomPrelude
  ( module Prelude
  , ByteString
  , LByteString
  , Text
  , Generic
  , fromMaybe
  , listToMaybe
  , foldl'
  , scanl'
  )
  where

import Prelude hiding (scanl, foldl)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (foldl', scanl')

type ByteString = B.ByteString
type LByteString = LB.ByteString
