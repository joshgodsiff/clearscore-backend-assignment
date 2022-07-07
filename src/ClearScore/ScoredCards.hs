{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ClearScore.ScoredCards where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API as S
import Servant.Client as S
import Data.Text

import qualified ClearScore.Types as T
import qualified ClearScore.Types as R (CreditCardRequest (..))
import qualified ClearScore.Types as C (CreditCard (..))

newtype ScoredCardsEndpoint = ScoredCardsEndpoint String

instance T.ParseUrl ScoredCardsEndpoint where
  parseUrl (ScoredCardsEndpoint url) = parseBaseUrl url

data CreditCard = CreditCard
  { card :: Text -- ^ Name of the credit card
  , apr :: Double -- ^ Annual percentage rate for the card
  , approvalRating :: Double -- ^ The likelihood of the user being approved, from 0.0 to 1.0
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | 
data ScoredCardsRequest = ScoredCardsRequest
  { name :: Text -- ^ Users name
  , score :: Int -- ^ Credit score between 0 and 700
  , salary :: Int -- ^ 
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype UserAgent = UserAgent { unUserAgent :: Text}

type ScoredCardsApi
    = "v2"
        :> "creditcards" 
          :> S.Header "User-Agent" Text
          :> ReqBody '[JSON] ScoredCardsRequest
          :> Post '[JSON] [CreditCard] 

creditcards :: Maybe Text -> ScoredCardsRequest -> ClientM [CreditCard]
creditcards = client (Proxy :: Proxy ScoredCardsApi)

instance T.Request ScoredCardsRequest CreditCard ScoredCardsEndpoint where
  call = creditcards $ Just "haskell/servant"
  
  convertRequest _ req = ScoredCardsRequest
    { name = R.name req
    , score = R.creditScore req
    , salary = R.salary req
    }

  convertResponse _ scoredCard = C.CreditCard
    { C.provider = "ScoredCards"
    , C.name = card scoredCard
    , C.apr = apr scoredCard
    , C.cardScore =
      let a = T.Apr $ apr scoredCard
          e = T.Eligibility $ approvalRating scoredCard * 100.0
        in T.unScore $ T.sortingScore e a
    }