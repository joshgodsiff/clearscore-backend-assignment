module ClearScore.Env
  ( Env (..)
  , HttpPort (..)
  , CsCardsEndpoint (..)
  , ScoredCardsEndpoint (..)
  , readEnvVars
  )
where

import ClearScore.CsCards (CsCardsEndpoint (..))
import ClearScore.ScoredCards (ScoredCardsEndpoint (..))
import Configuration.Dotenv
import qualified Data.Map as M
import Text.Read (readMaybe)

newtype HttpPort = HttpPort Int

data Env = Env
  { httpPort :: HttpPort
  , csCardsEndpoint :: CsCardsEndpoint
  , scoredCardsEndpoint :: ScoredCardsEndpoint
  }

readEnvVars :: IO (Maybe Env)
readEnvVars = do
  m <- M.fromList <$> loadFile defaultConfig
  pure $ Env
    <$> (M.lookup "HTTP_PORT" m >>= fmap HttpPort . readMaybe)
    <*> (CsCardsEndpoint <$> M.lookup "CSCARDS_ENDPOINT" m)
    <*> (ScoredCardsEndpoint <$> M.lookup "SCOREDCARDS_ENDPOINT" m)
