{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- This whole module is evil type-level voodoo.
-- Basically what we're doing here is associating a particular Url type with particular Request and Response types,
-- such that given a value of a particular Url type, we can automatically convert our public-facing Request type
-- (the one we get when someone calls this server) into the correct Request type for a particular backend
-- (e.g. CsCards or ScoredCards), make the call, and then automatically convert the Response type from the
-- particular backend call back into the response type for our frontend API.
module ClearScore.Types.Request
  ( ParseUrl (..)
  , Request (..)
  , Requestable (..)
  , DetermineEndpoint (..)
  , send
  )
where

import Control.Monad.Catch (MonadThrow)
import Servant.Client (BaseUrl, ClientM, ClientError, mkClientEnv, runClientM)
import ClearScore.Types.Internal
    ( CreditCardRequest
    , CreditCard
    )
import Servant.Server (ServerError, err500)
import Network.HTTP.Client (Manager)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)
import Data.Kind (Type)

-- Simple class to allow us to convert an arbitrary type into a URL that we can call.
class ParseUrl url where
  parseUrl :: MonadThrow m => url -> m BaseUrl

-- Associates a Request type with a Response type, and provides some functions for
-- calling the relevant backend, and converting its types to/from our API frontend types.
class Request req res | res -> req, req -> res where
  call :: req -> ClientM [res]
  convertRequest :: CreditCardRequest -> req
  convertResponse :: res -> CreditCard

-- This lets us get a heterogeneous (i.e. differently-typed) list of requestable URLs
data Requestable = forall url. (DetermineEndpoint url, ParseUrl url) => Requestable url

-- This associates a particular pair of request/response types to a particular URL type.
class Request (Req url) (Res url) => DetermineEndpoint url where
  type Req url :: Type
  type Res url :: Type

-- Bring it all together (i.e. actually make a call to a particular backend)
send 
  :: ParseUrl url
  => DetermineEndpoint url
  => Request (Req url) (Res url)
  => Manager
  -> CreditCardRequest 
  -> url
  -> IO (Either ServerError [CreditCard])
send mgr req u = do
  url <- parseUrl u
  let env = mkClientEnv mgr url

  res <- liftIO $ runClientM (call . carryReq u . convertRequest $ req) env
  let res' = convertResponse <$$> res
  pure . first clientErrorToServerError $ res'

-- Private --

-- This allows `send` to deduce what types it should be using.
carryReq :: DetermineEndpoint url => url -> Req url -> Req url
carryReq _ = id

infixl 4 <$$>

(<$$>)
  :: Functor f
  => Functor g
  => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap fmap fmap -- I heard you like fmap, so I put fmap in your fmap so you can fmap while you fmap. ^_^

clientErrorToServerError :: ClientError -> ServerError
clientErrorToServerError _ = err500