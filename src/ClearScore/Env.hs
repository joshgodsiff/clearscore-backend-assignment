{-# LANGUAGE ExistentialQuantification #-}

module ClearScore.Env
  ( Env (..)
  , HttpPort (..)
  , CsCardsEndpoint (..)
  , ScoredCardsEndpoint (..)
  , Requestable (..)
  , readEnvVars
  )
where

import ClearScore.CsCards (CsCardsEndpoint (..))
import ClearScore.ScoredCards (ScoredCardsEndpoint (..))
import Configuration.Dotenv
    ( defaultConfig,
      loadFile,
      Config(configExamplePath),
      onMissingFile )
import Configuration.Dotenv.Environment
import qualified Data.Map as M
import Text.Read (readMaybe)
import ClearScore.Types (DetermineEndpoint, ParseUrl)

newtype HttpPort = HttpPort Int

-- This lets us get a heterogeneous (i.e. differently-typed) list of requestable URLs
data Requestable = forall url. (DetermineEndpoint url, ParseUrl url) => Requestable url

data Env = Env 
  { httpPort :: HttpPort
  , backendUrls :: [Requestable]
  }

readEnvVars :: IO (Maybe Env)
readEnvVars = do
  let dotenvCfg = defaultConfig { configExamplePath = [".env.example"] } 
  _ <- loadFile dotenvCfg `onMissingFile` pure []
  env <- M.fromList <$> getEnvironment
  
  let requestableUrls = sequenceA 
        [ Requestable . CsCardsEndpoint     <$> M.lookup "CSCARDS_ENDPOINT" env
        , Requestable . ScoredCardsEndpoint <$> M.lookup "SCOREDCARDS_ENDPOINT" env
        ]

  pure $ Env
    <$> (M.lookup "HTTP_PORT" env >>= fmap HttpPort . readMaybe)
    <*> requestableUrls
