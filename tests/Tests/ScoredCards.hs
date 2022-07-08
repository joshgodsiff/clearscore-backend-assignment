{-# LANGUAGE OverloadedStrings #-}

module Tests.ScoredCards where

import Test.Hspec
import ClearScore.ScoredCards as Sc
import ClearScore.Types as T (CreditCard (..))
import ClearScore.Types (Request (convertResponse))

spec :: Spec
spec = do
  describe "ScoredCards" $ do
    it "Should match the examples in the assignment" $ do
      convertResponse scoredCard `shouldBe` creditCard

scoredCard :: Sc.CreditCard
scoredCard = Sc.CreditCard
  { Sc.card = "ScoredCard Builder"
  , Sc.apr = 19.4
  , Sc.approvalRating = 0.8
  }

creditCard :: T.CreditCard
creditCard = T.CreditCard
  { T.provider = "ScoredCards"
  , T.name = "ScoredCard Builder"
  , T.apr = 19.4
  , T.cardScore = 0.212
  }