{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getAccredidations
    , getAccredidationsSSL
    ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Monoid         (mempty)
import           Data.Text           (Text)
import           Network.API.Builder

-- Examples response:
--
-- [
--   {
--     "entity_type": "individual",
--     "legal_name": "Naval Ravikant",
--     "user_id": 155,
--     "basis": "income",
--     "expires_at": "2014-03-17T00:12:33Z",
--     "last_reviewed_at": "2013-12-17T00:12:33Z"
--   }
-- ]
--
data Accredidation = Accredidation { entityType     :: Text
                                   , legalName      :: Text
                                   , userId         :: Int
                                   , basis          :: Text
                                   , expiresAt      :: Text
                                   , lastReviewedAt :: Text
                                   }
  deriving (Show, Eq)

newtype Accredidations = Accredidations [Accredidation]
  deriving (Show, Eq)

instance FromJSON Accredidation where
  parseJSON (Object o) =
    Accredidation <$> o .: "entity_type"
                  <*> o .: "legal_name"
                  <*> o .: "user_id"
                  <*> o .: "basis"
                  <*> o .: "expires_at"
                  <*> o .: "last_reviewed_at"
  parseJSON _ = mempty

instance FromJSON Accredidations where
  parseJSON (Object o) = Accredidations <$> o .: "items"
  parseJSON _ = mempty

instance Receivable Accredidations where
  receive = useFromJSON

angelList :: Builder
angelList = basicBuilder "Angel.co API" "http://api.angel.co"

angelListSSL :: Builder
angelListSSL = basicBuilder "Angel.co API" "https://api.angel.co"

accredidationsRoute :: Route
accredidationsRoute = Route { urlPieces = [ "accredidations"
                                          ]
                            , urlParams = [ "access_token" =. ("XXX" :: Text)
                                          ]
                            , httpMethod = "GET"
                            }

getAccredidations :: IO (Either (APIError ()) Accredidations)
getAccredidations = execAPI angelList () $ runRoute accredidationsRoute

getAccredidationsSSL :: IO (Either (APIError ()) Accredidations)
getAccredidationsSSL = execAPI angelListSSL () $ runRoute accredidationsRoute
