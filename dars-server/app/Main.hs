{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Dars.API (serveAPI)
import Dars.API.Auth qualified as Auth
import Dars.Action qualified as Action
import Dars.Config (Context (..), activate, readConfig, showConfig)
import Dars.SQL qualified as SQL
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors qualified as Cors
import Network.Wai.Middleware.Gzip (GzipFiles (..), def, gzip, gzipFiles)
import Options.Generic
import Servant (Application)
import Servant.Server.Generic (genericServeTWithContext)
import System.IO

data Commands
  = Serve {config :: FilePath}
  | Test {config :: FilePath}
  deriving (Generic, Show)

instance ParseRecord Commands

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getRecord "csdc-server" >>= \case
    Serve configPath ->
      readConfig configPath >>= \case
        Left e ->
          error $ "Could not parse the configuration file: " <> e
        Right config -> do
          putStrLn "Starting the server with the following configuration:\n"
          showConfig config
          putStrLn ""
          context <- activate config
          putStrLn "Applying migrations..."
          migrate context
          mainWith context
    Test configPath ->
      readConfig configPath >>= \case
        Left e ->
          error $ "Could not parse the configuration file: " <> e
        Right config -> do
          Context {..} <- activate config
          Action.run_ dao $ do
            pure ()

mainWith :: Context -> IO ()
mainWith Context {..} = do
  putStrLn "Server ready."
  withStdoutLogger $ \logger -> do
    let settings = setPort port $ setLogger logger defaultSettings
    authSettings <- Auth.makeSettings jwkPath
    runSettings settings $
      middleware $
        application path authSettings dao

middleware :: Middleware
middleware =
  let corsOptions =
        Cors.simpleCorsResourcePolicy
          { Cors.corsRequestHeaders = Cors.simpleHeaders
          }
      cors = Cors.cors (\_ -> Just corsOptions)
      compress = gzip def {gzipFiles = GzipCompress}
   in compress . cors

application :: FilePath -> Auth.Settings -> Action.Context () -> Application
application path settings context =
  let sqlContext = context . sql
      cfg = Auth.makeContext settings
   in genericServeTWithContext (Action.run context) (serveAPI path sqlContext settings) cfg

migrate :: Context -> IO ()
migrate context = do
  let path = context . migration
  Action.run_ context . dao $ Action.runSQL $ SQL.migrate path
