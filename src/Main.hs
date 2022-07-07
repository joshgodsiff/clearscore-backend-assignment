import ClearScore.Servant
import Network.Wai.Handler.Warp
import ClearScore.Env (readEnvVars, HttpPort (..), Env (..))
import Control.Monad.IO.Class (MonadIO(..))

main :: IO ()
main = do
  envsM <- readEnvVars
  case envsM of
    Nothing -> error "Correct environment variables not set, please set HTTP_PORT, CSCARDS_ENDPOINT, and SCOREDCARDS_ENDPOINT"
    Just envs -> runServer envs

runServer :: Env -> IO ()
runServer env =
  let HttpPort port = httpPort env
  in liftIO $ run port (app env)


