{-# LANGUAGE NoImplicitPrelude #-}
module CustomPrelude
  ( module Prelude
  , module System.Directory
  , module Data.Time
  , readFile
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
  , readMay
  )
  where

import           Control.Monad.Trans  (MonadIO (..))
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Data.List            (foldl', scanl')
import           Data.Maybe           (fromMaybe, listToMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           Prelude              hiding (foldl, scanl, read, readFile)
import           System.Directory     (doesFileExist, getDirectoryContents,
                                       listDirectory)
import Data.Time (getCurrentTime)

type ByteString = B.ByteString
type LByteString = LB.ByteString

readFile :: FilePath -> IO LB.ByteString
readFile = LB.readFile

tshow :: Show a => a -> T.Text
tshow = T.pack . show

readMay :: Read a => String -> Maybe a
readMay s =
  case reads s of
       []         -> Nothing
       (a, _) : _ -> pure a
