import qualified Tests.Scoring
import qualified Tests.CsCards
import qualified Tests.ScoredCards
import qualified Tests.CreditCard

import Hedgehog.Main (defaultMain)
import Test.Hspec (hspec, Spec, describe)

main :: IO ()
main = do
  defaultMain 
    [ Tests.Scoring.tests
    ]
  hspec spec

spec :: Spec
spec = do
  describe "Tests.CsCards" Tests.CsCards.spec
  describe "Tests.ScoredCards" Tests.ScoredCards.spec
  describe "Tests.CreditCard" Tests.CreditCard.spec