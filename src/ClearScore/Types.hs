module ClearScore.Types 
  ( module T
  , Eligibility (..)
  , Apr (..)
  , Score (..)
  , sortingScore
) where

import ClearScore.Types.Internal as T
import ClearScore.Types.Request as T

-- These types simply exist to avoid accidentally putting the wrong Double in the wrong slot in `sortingScore`

newtype Eligibility = Eligibility { unEligibility :: Double } 
  deriving (Eq, Ord, Show)

newtype Apr = Apr { unApr :: Double } 
  deriving (Eq, Ord, Show)

newtype Score = Score { unScore :: Double } 
  deriving (Eq, Ord, Show)

sortingScore :: Eligibility -> Apr -> Score
sortingScore eligibility' apr' =
  let e = unEligibility eligibility'
      a = unApr apr'
  in Score $ e * ((1 / a) ** 2.0)
