{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module ClearScore.Servant where

import Network.Wai
import Servant
import ClearScore.Types
import ClearScore.Env
import ClearScore.Handlers
import Control.Monad.Reader (ReaderT (..), MonadReader, MonadIO (liftIO))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (MonadError, runExceptT, ExceptT)

-- | Servant type-level API, generated from the OpenAPI spec for ClearScore.
type ClearScoreAPI
    =    "creditcards" :> ReqBody '[JSON] CreditCardRequest :> Post '[JSON] [CreditCard] -- 'creditcardsPost' route

type AppM = ReaderT Env Handler

clearScoreApi :: Proxy ClearScoreAPI
clearScoreApi = Proxy

server :: (MonadThrow m, MonadReader Env m, MonadIO m, MonadError ServerError m) => ServerT ClearScoreAPI m
server = creditcardsPost

nt :: Env -> ReaderT Env (ExceptT ServerError IO) x -> Handler x
nt env x = do 
    res <- liftIO $ runExceptT $ runReaderT x env
    case res of
        Left e -> throwError e
        Right result -> pure result

app :: Env -> Application
app env =
    let api = hoistServer clearScoreApi (nt env) server
    in serve clearScoreApi api
        