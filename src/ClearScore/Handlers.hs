{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ClearScore.Handlers where

import ClearScore.Env
    ( Env(backendUrls)
    , Requestable(Requestable)
    ) 
import ClearScore.Types as C (CreditCard (..), CreditCardRequest (..))

import ClearScore.Types ( send )
import ClearScore.CsCards as CsCards ()
import ClearScore.ScoredCards ()
import Network.HTTP.Client     (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Except (liftEither, MonadError)
import Servant (ServerError (..))
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Catch (MonadThrow)
import Control.Monad (join)

creditcardsPost 
  :: MonadThrow m
  => MonadReader Env m
  => MonadIO m
  => MonadError ServerError m
  => C.CreditCardRequest
  -> m [C.CreditCard]
creditcardsPost req = do
  urls <- asks backendUrls
  manager <- liftIO $ newManager tlsManagerSettings

  let backendQueries = (\(Requestable url) -> send manager req url) <$> urls

  res <- liftIO $ mapConcurrently id backendQueries
  liftEither $ join <$> sequenceA res

