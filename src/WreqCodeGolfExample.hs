{-# LANGUAGE OverloadedStrings #-}

module WreqCodeGolfExample where

import Network.Wreq (Options, defaults, param, getWith, asValue, responseBody)
import Data.Text (Text)
import Control.Lens ((&), (.~), (^.), (^..))
import Data.Aeson.Lens (key, _Array, _String)
import Control.Arrow ((>>>), (&&&))

meetupEventsUrl :: String
meetupEventsUrl = "https://api.meetup.com/2/events"

-- | A valid Meetup group ID.
type GroupId = Text

-- | For searching for events in a Meetup group.
eventsOptions :: GroupId
              -> Options
eventsOptions groupId = defaults
  & param "format" .~ ["json"]
  & param "group_id" .~ [groupId]
  & param "status" .~ ["upcoming"]
  & param "order" .~ ["time"]
  & param "page" .~ ["10"]

-- | Code golf version. Don't do this?
getMeetupNameAndVenues :: GroupId -> IO [(Text, Text)]
getMeetupNameAndVenues groupId =
  getWith (eventsOptions groupId) meetupEventsUrl
  >>= asValue
  >>= ((^.. responseBody
        . key "results"
        . _Array . traverse)
       >>> map ((^. key "name" . _String)
                 &&& (^. key "venue"
                      . key "name" . _String)
                 )
       >>> return
      )
