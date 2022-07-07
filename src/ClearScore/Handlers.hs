{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ClearScore.Handlers where

import ClearScore.Env 
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
  cse <- asks csCardsEndpoint
  sce <- asks scoredCardsEndpoint
    
  -- You probably want to reuse the Manager across calls, for performance reasons
  manager <- liftIO $ newManager tlsManagerSettings

  let queryCsCards     = send manager cse req
  let queryScoredCards = send manager sce req

  res <- liftIO $ mapConcurrently id [queryCsCards, queryScoredCards]
  cards <- liftEither $ join <$> sequenceA res

  liftIO $ print cards

  pure cards

