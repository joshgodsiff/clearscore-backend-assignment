{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric              #-}

module ClearScore.Types.Internal where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)

data CreditCard = CreditCard
  { provider :: Text -- ^ Name of the partner that provides the credit card
  , name :: Text -- ^ Name of the credit card product
  , apr :: Double -- ^ Annual percentage rate for the card
  , cardScore :: Double -- ^ The score given to the credit card based on the scoring algorithm
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CreditCardRequest = CreditCardRequest
  { name :: Text -- ^ Users full name
  , creditScore :: Int -- ^ Credit score between 0 and 700
  , salary :: Int -- ^ Users annual salary
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Eligibility = Eligibility { unEligibility :: Double } 
  deriving (Eq, Ord, Show)

newtype Apr = Apr { unApr :: Double } 
  deriving (Eq, Ord, Show)

newtype Score = Score { unScore :: Double } 
  deriving (Eq, Ord, Show)