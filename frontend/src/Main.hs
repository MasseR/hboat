{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Component.Subscription           as Subscription
import           Control.Lens                     (set)
import           Data.Bifunctor                   (bimap)
import           Data.Generics.Product            (typed)
import           GHC.Generics                     (Generic)
import           Language.Javascript.JSaddle.Warp
import           Miso                             hiding (set)

data Action = NoOp
            | SubscriptionAction Subscription.Action

newtype Model = Model { subscription :: Subscription.Model } deriving (Eq, Generic)

updateModel :: Model -> Action -> Effect Action Model
updateModel m@Model{..} = \case
  NoOp -> noEff m
  SubscriptionAction act -> bimap SubscriptionAction (flip (set (typed @Subscription.Model)) m)
    $ Subscription.update subscription act

viewModel :: Model -> View Action
viewModel Model{..} =
  div_ [] [ SubscriptionAction <$> Subscription.view subscription ]

main :: IO ()
main = run 8081 $ startApp App{..}
  where
    model = Model Subscription.initModel
    initialAction = NoOp
    update = flip updateModel
    view = viewModel
    subs = []
    events = defaultEvents
    mountPoint = Nothing
