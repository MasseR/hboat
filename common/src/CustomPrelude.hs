{-# LANGUAGE NoImplicitPrelude #-}
module CustomPrelude
  ( module Prelude
  , ByteString
  , LByteString
  , Text
  )
  where

import Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)

type ByteString = B.ByteString
type LByteString = LB.ByteString
