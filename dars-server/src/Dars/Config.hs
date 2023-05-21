{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Dars.Config
  ( -- * Config
    Config (..),
    readConfig,
    showConfig,

    -- * Context
    Context (..),
    activate,
  )
where

import Dars.Action qualified as Action
import Dars.Prelude
import Dars.SQL qualified as SQL
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Text as Text
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- SQL Config

data SQLConfig = SQLConfigFile SQL.Config | SQLConfigEnv String
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

activateSQL :: SQLConfig -> IO SQL.Context
activateSQL (SQLConfigFile config) = SQL.activate config
activateSQL (SQLConfigEnv var) =
  env var >>= \case
    Nothing ->
      error $ "Could not find variable for SQL configuration named $" <> var
    Just str ->
      case SQL.parseURL (Text.unpack str) of
        Nothing ->
          error $ "Could not parse SQL configuration from $" <> var
        Just config ->
          SQL.activate config

--------------------------------------------------------------------------------
-- Config

data Config = Config
  { port :: Int,
    path :: FilePath,
    sql :: SQLConfig,
    migration :: FilePath,
    jwkPath :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

readConfig :: MonadIO m => FilePath -> m (Either String Config)
readConfig configPath = liftIO $ do
  mconfig <- eitherDecodeFileStrict configPath
  mport <- lookupEnv "PORT"
  case readMaybe =<< mport of
    Nothing ->
      pure mconfig
    Just (overridenPort :: Int) ->
      case mconfig of
        Left e ->
          pure $ Left e
        Right Config {..} ->
          pure $ Right Config {port = overridenPort, ..}

showConfig :: MonadIO m => Config -> m ()
showConfig config =
  let str = ByteString.unpack $ encodePretty config
   in liftIO $ putStrLn str

--------------------------------------------------------------------------------
-- Context

data Context = Context
  { port :: Int,
    path :: FilePath,
    dao :: Action.Context (),
    migration :: FilePath,
    jwkPath :: Maybe FilePath
  }
  deriving (Generic)

activate :: Config -> IO Context
activate config = do
  sql <- activateSQL config.sql
  pure
    Context
      { port = config.port,
        path = config.path,
        dao =
          Action.Context
            { Action.sql = sql,
              Action.user = ()
            },
        migration = config.migration,
        jwkPath = config.jwkPath
      }

--------------------------------------------------------------------------------
-- Helper

env :: String -> IO (Maybe Text)
env var = fmap Text.pack <$> lookupEnv var
