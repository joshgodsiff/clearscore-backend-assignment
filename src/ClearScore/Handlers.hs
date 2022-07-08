{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ClearScore.Handlers where

import ClearScore.Env
    ( Env(backendUrls)
    , Requestable(Requestable)
    ) 
import ClearScore.Types as C (CreditCard (..), CreditCardRequest (..))

import ClearScore.Types ( send )
import Network.HTTP.Client     (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Except (MonadError, liftEither)
import Servant (ServerError (..), err400)
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Catch (MonadThrow)
import Control.Monad (join)
import Data.Either (rights)
import Control.Concurrent (threadDelay)
import Data.Function ((&))
import Data.Maybe (catMaybes)

creditcardsPost 
  :: MonadThrow m
  => MonadReader Env m
  => MonadIO m
  => MonadError ServerError m
  => C.CreditCardRequest
  -> m [C.CreditCard]
creditcardsPost req = do
  _ <- liftEither $ validateRequest req
  urls <- asks backendUrls
  manager <- liftIO $ newManager tlsManagerSettings

  let backendQueries = (\(Requestable url) -> send manager req url) <$> urls

  -- If we had *lots* of backends (e.g. hundreds), we might need to be more careful about how many threads
  -- we're spawning here at one time. But given that we have two, this seems fine.
  res <- liftIO $ mapConcurrently (timeout (5 & seconds)) backendQueries

  -- We have an interesting choice here - we can either send a failure to the caller if any of the backends fail
  -- or we can ignore failures from the backend APIs and simply report all successes to the caller.
  -- I'm choosing the latter, mainly because if you scaled up this sort of system (e.g. to have dozens to hundreds
  -- of partner APIs that you're calling), it's expected that at least some of those calls would fail some of the time,
  -- and you wouldn't want to make your entire API unavailable because of that.
  -- We could also take some sort of middle ground, where we report both successes and failures, or fail the query only if
  -- all of the backends fail, but that's substantially more effort in both cases.

  pure . join . rights . catMaybes $ res

  -- This is the alternative (where we fail the whole query if any of the backends fails)
  --liftEither $ join <$> sequenceA res

validateRequest :: C.CreditCardRequest -> Either ServerError C.CreditCardRequest
validateRequest CreditCardRequest { creditScore = c } 
  | c < 0 || c > 700 = Left invalidParameters
validateRequest CreditCardRequest { salary = s }
  | s < 0 = Left invalidParameters
validateRequest req = Right req

invalidParameters :: ServerError
invalidParameters = err400 { errBody = "The request contained invalid parameters" }

seconds :: Int -> Int
seconds i = i * 1000000

-- This allows us to timeout a query that is taking too long, without killing the entire set of queries
timeout :: Int -> IO a -> IO (Maybe a)
timeout i f = maybeIso <$> race (threadDelay i) f
  where
    maybeIso :: Either () a -> Maybe a
    maybeIso (Right a) = Just a
    maybeIso _ = Nothing
