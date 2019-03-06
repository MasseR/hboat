{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TupleSections #-}
module HBoat.SQLite.Migrations where

import           CustomPrelude
import Control.Lens
import qualified Data.Text.Encoding        as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.QQ
import           System.FilePath           (takeExtension)

class Monad m => HasConnection m where
  getConnection :: m Connection

migrateFrom :: (MonadIO m, Monad m, HasConnection m) => FilePath -> m ()
migrateFrom path = do
  conn <- getConnection
  liftIO $ execute_ conn [sql|create table if not exists migrations (version, created)|]
  -- Failing to read directory is fine, this is run in the init step
  migrations <- augmentAll . filter ((==) ".sql" . takeExtension) <$> liftIO (listDirectory path)
  latest <- maybe 0 fromOnly . listToMaybe <$> liftIO (query_ conn [sql|select version from migrations order by version desc|])
  let newer = filter (\(v,_) -> v > latest) migrations
  mapM_ (liftIO . migrate conn) newer

  where
    parseVersion :: String -> Maybe Int
    parseVersion = readMay . fst . break (== '_')
    augment :: FilePath -> Maybe (Int, FilePath)
    augment p = (,p) <$> parseVersion p
    augmentAll :: [FilePath] -> [(Int, FilePath)]
    augmentAll = fromMaybe [] . sequenceA . fmap augment
    migrate conn (v,p) = do
      now <- getCurrentTime
      -- Failing to parse the query is fine, let the program break on it
      q <- Query . T.decodeUtf8 . view (from lazy) <$> readFile p
      execute_ conn q
      execute conn [sql|insert into migrations (version, created) values (?, ?)|] (v,now)

