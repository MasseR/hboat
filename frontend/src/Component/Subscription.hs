{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Component.Subscription where

import           Control.Comonad
import           Control.Monad.Trans (liftIO)
import           Data.Zipper         (Zipper)
import           GHC.Generics        (Generic)
import           Miso
import           Miso.String         (toMisoString)

data Action = FocusFeed String
            | FocusEntry String
            | NoOp

data Subscription = Subscription { link    :: String
                                 , title   :: String
                                 , entries :: Maybe (Zipper Entry) } deriving (Show, Generic, Eq)

data Entry = Entry { link    :: String
                   , title   :: String
                   , content :: String } deriving (Show, Generic, Eq)

newtype Model = Model { subscriptions :: Maybe (Zipper Subscription) } deriving (Show, Generic, Eq)

update :: Model -> Action -> Effect Action Model
update m = \case
  NoOp -> noEff m
  FocusFeed _ -> m <# (NoOp <$ liftIO (putStrLn "hello"))
  FocusEntry _ -> m <# (NoOp <$ liftIO (putStrLn "hello"))

view :: Model -> View Action
view Model{..} = div_ [ class_ "feeds" ]
      (  maybe [text "No feeds!"] renderFeeds subscriptions
      <> maybe [] renderEntries subscriptions )
  where
    renderEntries :: Zipper Subscription -> [View Action]
    renderEntries s = [ div_ [class_ "entries"] (maybe [] (foldMap renderEntry) (entries $ extract s)) ]
    renderEntry :: Entry -> [View Action]
    renderEntry Entry{..} = [ div_ [ class_ "entry" ] [ h2_ [ onClick (FocusEntry link)] [text (toMisoString title)], span_ [] []] ]
    renderFeeds :: Zipper Subscription -> [View Action]
    renderFeeds z = [ ul_ [] (foldMap renderFeed z) ]
    renderFeed :: Subscription -> [View Action]
    renderFeed Subscription{link,title} = [ li_ [] [ h3_ [onClick (FocusFeed link)] [text (toMisoString title)] ] ]

initModel :: Model
initModel = Model{..}
  where
  -- for dev purposes hardcode a list of feeds
  subscriptions = Just $ pure $ Subscription "http://example.com" "Example feed" Nothing
