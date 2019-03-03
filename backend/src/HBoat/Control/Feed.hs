module HBoat.Control.Feed where

import           CustomPrelude
import qualified Text.Feed.Import as Feed
import           Text.Feed.Types

newtype URL = URL String

data FeedError = DecodeFailure

class Monad m => MonadFeed m where
  fetchFeed :: URL -> m (Either FeedError Feed)

parseFeed :: LByteString -> Either FeedError Feed
parseFeed = maybe (Left DecodeFailure) Right . Feed.parseFeedSource