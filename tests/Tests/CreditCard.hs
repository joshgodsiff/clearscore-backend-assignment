{-# LANGUAGE OverloadedStrings #-}

module Tests.CreditCard where

import Test.Hspec
import ClearScore.Types (CreditCardRequest (..))
import Data.Either (isRight)
import ClearScore.Handlers (validateRequest, invalidParameters)

spec :: Spec
spec = do
  describe "Request validation" $ do
    it "Should return a 400 error if the credit score is below 0" $ do
      validateRequest creditScoreTooLow `shouldBe` Left invalidParameters

    it "Should return a 400 error if the credit score is above 700" $ do
      validateRequest creditScoreTooHigh `shouldBe` Left invalidParameters

    it "Should return a 400 error if the salary is below 0" $ do
      validateRequest salaryTooLow `shouldBe` Left invalidParameters

    it "Should not return an error if the credit score is between 0 and 700 (inclusive), and the salary is above 0" $ do
      validateRequest goldilocks `shouldSatisfy` isRight

creditScoreTooLow :: CreditCardRequest
creditScoreTooLow = CreditCardRequest
  { name = "John Smith"
  , creditScore = -42
  , salary = 42
  }

creditScoreTooHigh :: CreditCardRequest
creditScoreTooHigh = CreditCardRequest
  { name = "John Smith"
  , creditScore = 742
  , salary = 42
  }

salaryTooLow :: CreditCardRequest
salaryTooLow = CreditCardRequest
  { name = "John Smith"
  , creditScore = 42
  , salary = -42
  }

goldilocks :: CreditCardRequest
goldilocks = CreditCardRequest
  { name = "John Smith"
  , creditScore = 500
  , salary = 28000
  }