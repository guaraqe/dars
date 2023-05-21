{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dars.API.Common
  ( API,
    NamedAPI,
    serveAPI,
  )
where

import Dars.Action
import Dars.Prelude
import Servant hiding (Post)
import Servant qualified

--------------------------------------------------------------------------------
-- Synonyms

type GetJSON a = Get '[JSON] a

type PostJSON a b = ReqBody '[JSON] a :> Servant.Post '[JSON] b

type DeleteJSON a = Delete '[JSON] a

type CaptureId a = Capture "id" (Id a)

--------------------------------------------------------------------------------
-- User API

data UserAPI mode = UserAPI
  { getUserInfo :: mode :- "info" :> GetJSON (Maybe User)
  }
  deriving (Generic)

userAPI :: ServerAuth UserAPI
userAPI =
  UserAPI
    { getUserInfo = undefined
    }

--------------------------------------------------------------------------------
-- API

type API = NamedRoutes NamedAPI

data NamedAPI mode = NamedAPI
  { userAPI :: mode :- "user" :> NamedRoutes UserAPI
  }
  deriving (Generic)

serveAPI :: ServerAuth NamedAPI
serveAPI =
  NamedAPI
    { userAPI
    }
