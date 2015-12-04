{-# LANGUAGE OverloadedStrings #-}

module WreqExampleSpec where

import WreqExample (GroupId, eventName, venueName, getMeetupEventInfos)
import Test.Hspec ( Spec, hspec, describe, it
                  , shouldSatisfy, shouldNotSatisfy, pendingWith
                  )
import qualified Data.Text as Text

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "wreq" $ do
    it "there are named, located Pittsburgh Code and Supply events coming up" $ do
      -- Warning! This is a stateful test going out to the Web.
      -- I've disabled it. You can comment out the line below to enable it.
      pendingWith "disabled to avoid going out to the Web for everyone running this code"

      events <- getMeetupEventInfos pittsburghCodeAndSupplyId
      events `shouldNotSatisfy` null
      events `shouldSatisfy` any
        (\event -> (not . Text.null . eventName) event
                   && (not . Text.null . venueName) event)

-- | <http://www.meetup.com/Pittsburgh-Code-Supply/ Pittsburgh Code and Supply>
pittsburghCodeAndSupplyId :: GroupId
pittsburghCodeAndSupplyId = "13452572"

-- | <http://www.meetup.com/Pittsburgh-Functional-Programming-Meetup Pittsburgh Functional Programming>
pittsburghFunctionalProgrammingId :: GroupId
pittsburghFunctionalProgrammingId = "18680854"

main :: IO ()
main = hspec spec
