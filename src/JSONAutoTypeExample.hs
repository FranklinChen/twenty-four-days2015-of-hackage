{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module JSONAutoTypeExample where

-- Reuse what we did earlier.
import WreqExample (EventInfo(..), GroupId, getMeetupEventsJSONBytes)

-- Module automatically generated using json-autotype.
import qualified MeetupEventsJSON as Meetup
import qualified Data.Aeson as Aeson
import Control.Arrow ((>>>))
import Data.Aeson.AutoType.Alternative ((:|:)(AltLeft, AltRight))
import qualified Data.Text as Text

getMeetupEventInfos :: GroupId -> IO (Either String [EventInfo])
getMeetupEventInfos groupId =
  getMeetupEventsJSONBytes groupId
  >>= (Aeson.eitherDecode
       >>> fmap extractEventInfos
       >>> return
      )

extractEventInfos :: Meetup.TopLevel -> [EventInfo]
extractEventInfos =
  Meetup.topLevelResults
  >>> map extractEventInfo

extractEventInfo :: Meetup.ResultsElt -> EventInfo
extractEventInfo event =
  EventInfo { eventName = Meetup.resultsEltName event
            , venueName = extractVenueName (Meetup.resultsEltVenue event)
            }

-- | Trickier because json-autotype apparently found events without a venue.
extractVenueName :: Maybe (Meetup.Venue :|: [Maybe Aeson.Value]) -> Text.Text
extractVenueName Nothing = ""
extractVenueName (Just (AltLeft venue)) = Meetup.venueName venue
extractVenueName (Just (AltRight jsonValues)) =
  Text.pack ("(unexpected JSON venue: " ++ show jsonValues ++ ")")
