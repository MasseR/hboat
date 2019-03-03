{-# LANGUAGE NoImplicitPrelude #-}
module CustomPrelude
  ( module Prelude
  , ByteString
  , LByteString
  , Text
  , Generic
  , fromMaybe
  , listToMaybe
  )
  where

import Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, listToMaybe)

type ByteString = B.ByteString
type LByteString = LB.ByteString
