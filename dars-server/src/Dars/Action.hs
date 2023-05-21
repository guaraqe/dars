{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Dars.Action
  ( -- * Action
    Action,
    ActionAuth,
    run,
    run_,
    withUser,
    throw,

    -- * Error
    Error (..),

    -- * Context
    Context (..),

    -- * Server
    Server,
    ServerAuth,

    -- * SQL
    runSQL,
    runQuery,
  )
where

import Control.Exception (Exception, try)
import Control.Monad.Except (MonadError (..), throwError)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Dars.Prelude
import Dars.SQL qualified as SQL
import Hasql.Statement (Statement)
import Servant (ServerError, err401, err500)
import Servant.Server.Generic (AsServerT)
import UnliftIO (MonadUnliftIO, throwIO)

--------------------------------------------------------------------------------
-- Context

data Context user = Context
  { sql :: SQL.Context,
    user :: user
  }
  deriving (Generic)

--------------------------------------------------------------------------------
-- Error

data Error
  = ErrorSQL SQL.Error
  | Unauthorized
  deriving (Show, Eq)

instance Exception Error

throw :: Error -> Action user a
throw err =
  let serverErr :: ServerError
      serverErr = case err of
        Unauthorized -> err401
        _ -> err500
   in throwIO serverErr

--------------------------------------------------------------------------------
-- Server

type Server api = api (AsServerT (Action ()))

type ServerAuth api = api (AsServerT (Action (Id User)))

--------------------------------------------------------------------------------
-- Action

newtype Action user a = Action (ReaderT (Context user) IO a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Context user),
      MonadIO,
      MonadUnliftIO
    )

-- Actions with authentication needed
type ActionAuth = Action (Id User)

run ::
  (MonadIO m, MonadError ServerError m) =>
  Context user ->
  Action user a ->
  m a
run ctx (Action act) =
  liftIO (try (runReaderT act ctx)) >>= \case
    Left e -> throwError e
    Right a -> pure a

run_ :: Context user -> Action user a -> IO a
run_ ctx (Action act) = runReaderT act ctx

withUser :: Id User -> ActionAuth a -> Action () a
withUser pid (Action (ReaderT act)) =
  Action $ ReaderT $ \ctx -> act $ ctx {user = pid}

runSQL :: SQL.Action a -> Action user a
runSQL act = do
  ctx <- asks (.sql)
  SQL.run ctx act >>= \case
    Left e ->
      liftIO $ throwIO $ ErrorSQL e
    Right a ->
      pure a

runQuery :: Statement a b -> a -> Action user b
runQuery statement = runSQL . SQL.query statement
