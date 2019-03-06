{-# LANGUAGE NoImplicitPrelude #-}
module CustomPrelude
  ( module Prelude
  , module System.Directory
  , MonadIO(..)
  , ByteString
  , LByteString
  , Text
  , Generic
  , fromMaybe
  , listToMaybe
  , foldl'
  , scanl'
  , tshow
  )
  where

import           Control.Monad.Trans  (MonadIO (..))
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Data.List            (foldl', scanl')
import           Data.Maybe           (fromMaybe, listToMaybe)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Prelude              hiding (foldl, scanl)
import           System.Directory     (doesFileExist, getDirectoryContents,
                                       listDirectory)
import qualified Data.Text as T

type ByteString = B.ByteString
type LByteString = LB.ByteString

tshow :: Show a => a -> T.Text
tshow = T.pack . show
