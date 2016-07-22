{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module MeetupEventsJSON where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import           GHC.Generics hiding (Meta)

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data Group = Group { 
    groupCreated :: Double,
    groupWho :: Text,
    groupJoinMode :: Text,
    groupGroupLat :: Double,
    groupUrlname :: Text,
    groupName :: Text,
    groupGroupLon :: Double,
    groupId :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Group where
  parseJSON (Object v) = Group <$> v .:   "created" <*> v .:   "who" <*> v .:   "join_mode" <*> v .:   "group_lat" <*> v .:   "urlname" <*> v .:   "name" <*> v .:   "group_lon" <*> v .:   "id"
  parseJSON _          = mzero


instance ToJSON Group where
  toJSON     (Group {..}) = object ["created" .= groupCreated, "who" .= groupWho, "join_mode" .= groupJoinMode, "group_lat" .= groupGroupLat, "urlname" .= groupUrlname, "name" .= groupName, "group_lon" .= groupGroupLon, "id" .= groupId]


data Venue = Venue { 
    venueRepinned :: Bool,
    venueState :: Text,
    venueCountry :: Text,
    venueZip :: (Maybe (Text:|:[(Maybe Value)])),
    venueLat :: Double,
    venueName :: Text,
    venueCity :: Text,
    venueId :: Double,
    venueLon :: Double,
    venueAddress1 :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON Venue where
  parseJSON (Object v) = Venue <$> v .:   "repinned" <*> v .:   "state" <*> v .:   "country" <*> v .:?? "zip" <*> v .:   "lat" <*> v .:   "name" <*> v .:   "city" <*> v .:   "id" <*> v .:   "lon" <*> v .:   "address_1"
  parseJSON _          = mzero


instance ToJSON Venue where
  toJSON     (Venue {..}) = object ["repinned" .= venueRepinned, "state" .= venueState, "country" .= venueCountry, "zip" .= venueZip, "lat" .= venueLat, "name" .= venueName, "city" .= venueCity, "id" .= venueId, "lon" .= venueLon, "address_1" .= venueAddress1]


data Fee = Fee { 
    feeAmount :: Double,
    feeRequired :: Text,
    feeCurrency :: Text,
    feeAccepts :: Text,
    feeDescription :: Text,
    feeLabel :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON Fee where
  parseJSON (Object v) = Fee <$> v .:   "amount" <*> v .:   "required" <*> v .:   "currency" <*> v .:   "accepts" <*> v .:   "description" <*> v .:   "label"
  parseJSON _          = mzero


instance ToJSON Fee where
  toJSON     (Fee {..}) = object ["amount" .= feeAmount, "required" .= feeRequired, "currency" .= feeCurrency, "accepts" .= feeAccepts, "description" .= feeDescription, "label" .= feeLabel]


data ResultsElt = ResultsElt { 
    resultsEltStatus :: Text,
    resultsEltGroup :: Group,
    resultsEltTime :: Double,
    resultsEltWaitlistCount :: Double,
    resultsEltVenue :: (Maybe (Venue:|:[(Maybe Value)])),
    resultsEltCreated :: Double,
    resultsEltUtcOffset :: Double,
    resultsEltEventUrl :: Text,
    resultsEltYesRsvpCount :: Double,
    resultsEltHeadcount :: Double,
    resultsEltFee :: (Maybe (Fee:|:[(Maybe Value)])),
    resultsEltVisibility :: Text,
    resultsEltMaybeRsvpCount :: Double,
    resultsEltName :: Text,
    resultsEltId :: Text,
    resultsEltRsvpLimit :: (Maybe (Double:|:[(Maybe Value)])),
    resultsEltUpdated :: Double,
    resultsEltDuration :: (Maybe (Double:|:[(Maybe Value)])),
    resultsEltDescription :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON ResultsElt where
  parseJSON (Object v) = ResultsElt <$> v .:   "status" <*> v .:   "group" <*> v .:   "time" <*> v .:   "waitlist_count" <*> v .:?? "venue" <*> v .:   "created" <*> v .:   "utc_offset" <*> v .:   "event_url" <*> v .:   "yes_rsvp_count" <*> v .:   "headcount" <*> v .:?? "fee" <*> v .:   "visibility" <*> v .:   "maybe_rsvp_count" <*> v .:   "name" <*> v .:   "id" <*> v .:?? "rsvp_limit" <*> v .:   "updated" <*> v .:?? "duration" <*> v .:?? "description"
  parseJSON _          = mzero


instance ToJSON ResultsElt where
  toJSON     (ResultsElt {..}) = object ["status" .= resultsEltStatus, "group" .= resultsEltGroup, "time" .= resultsEltTime, "waitlist_count" .= resultsEltWaitlistCount, "venue" .= resultsEltVenue, "created" .= resultsEltCreated, "utc_offset" .= resultsEltUtcOffset, "event_url" .= resultsEltEventUrl, "yes_rsvp_count" .= resultsEltYesRsvpCount, "headcount" .= resultsEltHeadcount, "fee" .= resultsEltFee, "visibility" .= resultsEltVisibility, "maybe_rsvp_count" .= resultsEltMaybeRsvpCount, "name" .= resultsEltName, "id" .= resultsEltId, "rsvp_limit" .= resultsEltRsvpLimit, "updated" .= resultsEltUpdated, "duration" .= resultsEltDuration, "description" .= resultsEltDescription]


data Meta = Meta { 
    metaNext :: Text,
    metaLink :: Text,
    metaUrl :: Text,
    metaCount :: Double,
    metaTotalCount :: Double,
    metaLat :: Text,
    metaMethod :: Text,
    metaId :: Text,
    metaLon :: Text,
    metaUpdated :: Double,
    metaTitle :: Text,
    metaDescription :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON Meta where
  parseJSON (Object v) = Meta <$> v .:   "next" <*> v .:   "link" <*> v .:   "url" <*> v .:   "count" <*> v .:   "total_count" <*> v .:   "lat" <*> v .:   "method" <*> v .:   "id" <*> v .:   "lon" <*> v .:   "updated" <*> v .:   "title" <*> v .:   "description"
  parseJSON _          = mzero


instance ToJSON Meta where
  toJSON     (Meta {..}) = object ["next" .= metaNext, "link" .= metaLink, "url" .= metaUrl, "count" .= metaCount, "total_count" .= metaTotalCount, "lat" .= metaLat, "method" .= metaMethod, "id" .= metaId, "lon" .= metaLon, "updated" .= metaUpdated, "title" .= metaTitle, "description" .= metaDescription]


data TopLevel = TopLevel { 
    topLevelResults :: [ResultsElt],
    topLevelMeta :: Meta
  } deriving (Show,Eq,Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "results" <*> v .:   "meta"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["results" .= topLevelResults, "meta" .= topLevelMeta]




parse :: FilePath -> IO TopLevel
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just v  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


