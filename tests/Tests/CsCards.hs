{-# LANGUAGE OverloadedStrings #-}

module Tests.CsCards where

import Test.Hspec
import ClearScore.CsCards as Cs
import ClearScore.Types as T (CreditCard (..))
import ClearScore.Types (Request (convertResponse))

spec :: Spec
spec = do
  describe "CsCards" $ do
    it "Should match the examples in the assignment" $ do
      (convertResponse <$> [card1, card2]) `shouldBe` [creditCard1, creditCard2]

card1 :: Cs.Card
card1 = Cs.Card
  { Cs.cardName = "SuperSaver Card"
  , Cs.apr = 21.4
  , Cs.eligibility = 6.3
  }

card2 :: Cs.Card
card2 = Cs.Card
  { Cs.cardName = "SuperSpender Card"
  , Cs.apr = 19.2
  , Cs.eligibility = 5.0
  }

creditCard1 :: T.CreditCard
creditCard1 = T.CreditCard
  { T.provider = "CSCards"
  , T.name = "SuperSaver Card"
  , T.apr = 21.4
  , T.cardScore = 0.137
  }

creditCard2 :: T.CreditCard
creditCard2 = T.CreditCard
  { T.provider = "CSCards"
  , T.name = "SuperSpender Card"
  , T.apr = 19.2
  , T.cardScore = 0.135
  }