{-# LANGUAGE TemplateHaskell #-}

module Tests.Scoring where

import Data.List (sort)
import ClearScore.Types
import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

-- If we hold Apr constant, then Score should be directly proportional to Eligibility
prop_score_proportional_to_eligibility :: Property
prop_score_proportional_to_eligibility = property $ do
  es <- forAll $ G.list (R.linear 0 100) genEligibility
  a <- forAll genApr

  let sorted = sort es
  let scored = flip sortingScore a <$> sorted

  scored === sort scored

-- If we hold Eligibility constant, Score should be inversely proportional to Apr
prop_score_inversely_proportional_to_apr :: Property
prop_score_inversely_proportional_to_apr = property $ do
  as <- forAll $ G.list (R.linear 0 100) genApr
  e <- forAll genEligibility

  let sorted = sort as
  let scored = sortingScore e <$> sorted

  reverse scored === sort scored

genEligibility :: Gen Eligibility
genEligibility = 
  Eligibility <$> G.double (R.exponentialFloat 0 100)

genApr :: Gen Apr
genApr =
  Apr <$> G.double (R.exponentialFloat 0 100)

tests :: IO Bool
tests = checkParallel $$(discover)
