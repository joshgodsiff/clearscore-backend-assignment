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
import ClearScore.Types (Requestable (..))

newtype HttpPort = HttpPort Int

data Env = Env 
  { httpPort :: HttpPort
  , backendUrls :: [Requestable]
  }

readEnvVars :: IO (Maybe Env)
readEnvVars = do
  m <- M.fromList <$> loadFile defaultConfig
  
  let requestableUrls = sequenceA 
        [ Requestable . CsCardsEndpoint     <$> M.lookup "CSCARDS_ENDPOINT" m
        , Requestable . ScoredCardsEndpoint <$> M.lookup "SCOREDCARDS_ENDPOINT" m
        ] 

  pure $ Env
    <$> (M.lookup "HTTP_PORT" m >>= fmap HttpPort . readMaybe)
    <*> requestableUrls
