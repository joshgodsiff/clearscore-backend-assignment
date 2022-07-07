{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ClearScore.Types.Request
  ( ParseUrl (..)
  , Request (..)
  , send
  )
where

import Control.Monad.Catch (MonadThrow)
import Servant.Client (BaseUrl, ClientM, ClientError, mkClientEnv, runClientM)
import ClearScore.Types.Internal
    ( CreditCardRequest
    , CreditCard
    )
import qualified Data.Data as P
import Servant.Server (ServerError, err500)
import Network.HTTP.Client (Manager)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)

class ParseUrl url where
  parseUrl :: MonadThrow m => url -> m BaseUrl

class ParseUrl url => Request req res url | url -> res req, res -> url req, req -> url res where
  call :: req -> ClientM [res]
  convertRequest :: P.Proxy req -> CreditCardRequest -> req
  convertResponse :: P.Proxy res -> res -> CreditCard

send :: Request req res url => Manager -> url -> CreditCardRequest -> IO (Either ServerError [CreditCard])
send mgr u req = do
  url <- parseUrl u
  let env = mkClientEnv mgr url

  res <- liftIO $ runClientM (call $ convertRequest (reqProxy u) req) env
  let foo = convertResponse (resProxy u) <$$> res
  pure . first clientErrorToServerError $ foo

-- Private --

reqProxy :: Request req res url => url -> P.Proxy req
reqProxy _ = P.Proxy

resProxy :: Request req res url => url -> P.Proxy res
resProxy _ = P.Proxy

infixl 4 <$$>

(<$$>)
  :: Functor f
  => Functor g
  => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap fmap fmap -- I heard you like fmap, so I put fmap in your fmap so you can fmap while you fmap. ^_^

clientErrorToServerError :: ClientError -> ServerError
clientErrorToServerError _ = err500