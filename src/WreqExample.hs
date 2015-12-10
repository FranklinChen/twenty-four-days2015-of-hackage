{-# LANGUAGE OverloadedStrings #-}

module WreqExample where

import Network.Wreq (Options, defaults, param, getWith, asValue, responseBody)
import Data.Text (Text)
import Data.Aeson (Value)
import Control.Lens (view, set, toListOf)
import Data.Aeson.Lens (key, _Array, _String)

-- | Information that we care about from a Meetup event.
data EventInfo =
  EventInfo { eventName :: Text
            , venueName :: Text
            }
  deriving (Show)

-- | A valid Meetup group ID.
type GroupId = Text

meetupEventsUrl :: String
meetupEventsUrl = "https://api.meetup.com/2/events"

-- | For searching for events in a Meetup group.
--
-- The use of normal function application syntax is deliberate.
-- See 'WreqCodeGolfExample' for the idiomatic operator-based syntax.
eventsOptions :: GroupId
              -> Options
eventsOptions groupId =
  set (param "page") ["10"] (
    set (param "order") ["time"] (
      set (param "status") ["upcoming"] (
        set (param "group_id") [groupId] (
          set (param "format") ["json"] defaults))))

-- | Get useful information about upcoming Meetup events.
getMeetupEventInfos :: GroupId -> IO [EventInfo]
getMeetupEventInfos groupId = do
  -- Go out to Web to receive a lazy ByteString.
  response <- getWith (eventsOptions groupId) meetupEventsUrl

  -- Parse the ByteString response, including headers and body,
  -- into an untyped JSON object.
  jsonResponse <- asValue response

  -- Extract the list of events.
  let events = toListOf (responseBody
                         . key "results"
                         . _Array . traverse
                        ) jsonResponse

  -- For each event, extract its name and the name of its venue.
  return (map jsonToEventInfo events)

-- | Extract our typed data model from an untyped JSON object.
jsonToEventInfo :: Value -> EventInfo
jsonToEventInfo json =
  EventInfo { eventName = view (key "name" . _String) json
            , venueName = view (key "venue"
                                . key "name" . _String) json
            }
