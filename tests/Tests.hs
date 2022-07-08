import qualified Tests.Scoring
import qualified Tests.CsCards
import qualified Tests.ScoredCards

import Hedgehog.Main (defaultMain)
import Test.Hspec (hspec)
import Data.Foldable (traverse_)

main :: IO ()
main = do
  defaultMain 
    [ Tests.Scoring.tests
    ]
  traverse_ hspec 
    [ Tests.CsCards.spec
    , Tests.ScoredCards.spec
    ]
