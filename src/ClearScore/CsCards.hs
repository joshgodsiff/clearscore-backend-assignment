{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ClearScore.CsCards where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API as S
import Servant.Client as S
import Data.Text

import qualified ClearScore.Types as T
import qualified ClearScore.Types as R (CreditCardRequest (..))
import qualified ClearScore.Types as C (CreditCard (..))

type CsCardsApi
    = "v1" 
        :> "cards" 
          :> S.Header "User-Agent" Text
          :> ReqBody '[JSON] CardSearchRequest
          :> Post '[JSON] [Card] -- 'v1CardsPost' route

cards :: Maybe Text -> CardSearchRequest -> ClientM [Card]
cards = S.client (Proxy :: Proxy CsCardsApi)

newtype CsCardsEndpoint = CsCardsEndpoint String

instance T.ParseUrl CsCardsEndpoint where
  parseUrl (CsCardsEndpoint url) = parseBaseUrl url

instance T.DetermineEndpoint CsCardsEndpoint where
  type Req CsCardsEndpoint = CardSearchRequest
  type Res CsCardsEndpoint = Card

data Card = Card
  { cardName :: Text -- ^ Name of the credit card product
  , apr :: Double -- ^ Annual percentage rate for the card
  , eligibility :: Double -- ^ How likely the user is to be approved ranging from 0.0 to 10.0
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CardSearchRequest = CardSearchRequest
  { name :: Text -- ^ Users full name
  , creditScore :: Int -- ^ Credit score between 0 and 700
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance T.Request CardSearchRequest Card where
  call = cards $ Just "haskell/servant"
  
  convertRequest req = CardSearchRequest
    { name = R.name req
    , creditScore = R.creditScore req
    }

  convertResponse csCard = C.CreditCard
    { C.provider = "CSCards"
    , C.name = cardName csCard
    , C.apr = apr csCard
    , C.cardScore =
      let a = T.Apr $ apr csCard
          e = T.Eligibility $ eligibility csCard * 10.0 -- / 10.0
        in T.unScore $ T.sortingScore e a
    }
