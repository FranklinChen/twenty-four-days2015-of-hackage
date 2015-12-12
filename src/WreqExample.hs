{-# LANGUAGE OverloadedStrings #-}

module WreqExample where

import Network.Wreq (Options, defaults, param, getWith, asValue, responseBody)
import Data.Text (Text)
import Data.Aeson (Value)
import Control.Lens (view, set, toListOf)
import Data.Aeson.Lens (key, _Array, _String)
import qualified Data.ByteString.Lazy as BSL

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

-- | Same thing, but refactored using composition operator.
eventsOptionsRefactored :: GroupId -> Options
eventsOptionsRefactored groupId = builder defaults
  where builder = eventsOptionsBuilder groupId

-- | Recall: type is sugar for GroupId -> (Options -> Options)
eventsOptionsBuilder :: GroupId -> Options -> Options
eventsOptionsBuilder groupId =
  set (param "page") ["10"]
  . set (param "order") ["time"]
  . set (param "status") ["upcoming"]
  . set (param "group_id") [groupId]
  . set (param "format") ["json"]

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

-- | Convenient for just getting the raw JSON back.
getMeetupEventsJSONBytes :: GroupId -> IO BSL.ByteString
getMeetupEventsJSONBytes groupId =
  getWith (eventsOptions groupId) meetupEventsUrl
  >>= (return . view responseBody)
